{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StrictData          #-}

module Graphex.Hpack
  ( discoverHpackModules
  , discoverHpackModuleGraph
  , HpackDiscoverOpts (..)
  , pathToModuleName
  , findHsFiles
  ) where

import           Graphex.Cabal (CabalDiscoverType (..), CabalGraph (..),
                                CabalUnit (..), Discovery (..),
                                buildModuleGraph, discoversUnit)
import           Graphex.Core

import           Control.Monad          (forM)
import           Data.Aeson             (FromJSON (..), Value (..), withObject,
                                         (.:?))
import           Data.Aeson.Types       (Parser, typeMismatch)
import           Data.List              (isPrefixOf, isSuffixOf)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (catMaybes, fromMaybe)
import           Data.Semigroup.Foldable
import           Data.String            (fromString)
import qualified Data.Text              as T
import           Data.Yaml.Include      (decodeFileEither)
import           System.Directory       (doesDirectoryExist, doesFileExist,
                                         listDirectory)
import           System.FilePath        (dropExtension, makeRelative,
                                         normalise, (</>))
import           UnliftIO.Async         (pooledMapConcurrentlyN)

-- | Top-level package.yaml structure
data PackageYaml = PackageYaml
  { pyLibrary           :: Maybe ComponentConfig
  , pyInternalLibraries :: Map String ComponentConfig
  , pyExecutables       :: Map String ExecutableConfig
  , pyTests             :: Map String ExecutableConfig
  }

data ComponentConfig = ComponentConfig
  { ccSourceDirs     :: [FilePath]
  , ccExposedModules :: Maybe [String]
  , ccOtherModules   :: Maybe [String]
  }

data ExecutableConfig = ExecutableConfig
  { ecSourceDirs    :: [FilePath]
  , ecMain          :: Maybe String
  , ecOtherModules  :: Maybe [String]
  }

-- | Handles hpack's flexible source-dirs: can be a single string or a list
parseSourceDirs :: Value -> Parser [FilePath]
parseSourceDirs (String s) = pure [T.unpack s]
parseSourceDirs (Array a)  = traverse parseJSON (foldr (:) [] a)
parseSourceDirs v          = typeMismatch "String or Array" v

instance FromJSON PackageYaml where
  parseJSON = withObject "PackageYaml" $ \o -> do
    pyLibrary <- o .:? "library"
    pyInternalLibraries <- fromMaybe mempty <$> o .:? "internal-libraries"
    pyExecutables <- fromMaybe mempty <$> o .:? "executables"
    pyTests <- fromMaybe mempty <$> o .:? "tests"
    pure PackageYaml{..}

instance FromJSON ComponentConfig where
  parseJSON = withObject "ComponentConfig" $ \o -> do
    ccSourceDirs <- fromMaybe ["."] <$> (o .:? "source-dirs" >>= traverse parseSourceDirs)
    ccExposedModules <- o .:? "exposed-modules"
    ccOtherModules <- o .:? "other-modules"
    pure ComponentConfig{..}

instance FromJSON ExecutableConfig where
  parseJSON = withObject "ExecutableConfig" $ \o -> do
    ecSourceDirs <- fromMaybe ["."] <$> (o .:? "source-dirs" >>= traverse parseSourceDirs)
    ecMain <- o .:? "main"
    ecOtherModules <- o .:? "other-modules"
    pure ExecutableConfig{..}

data HpackDiscoverOpts = HpackDiscoverOpts
  { hpackToDiscover      :: NonEmpty CabalDiscoverType
  , hpackIncludeExternal :: Bool
  , hpackNumJobs         :: Int
  , hpackPruneTo         :: Maybe (ModuleName -> Bool)
  }

-- | Convert a file path relative to a source directory into a module name.
-- e.g. pathToModuleName "src" "src/Graphex/Core.hs" == "Graphex.Core"
pathToModuleName :: FilePath -> FilePath -> ModuleName
pathToModuleName srcDir fp =
    fromString $ map (\c -> if c == '/' || c == '\\' then '.' else c) $ dropExtension relative
  where
    relative = makeRelative srcDir fp

-- | Recursively find all .hs files under a directory, excluding specified subdirectories.
findHsFiles :: FilePath -> IO [FilePath]
findHsFiles dir = findHsFilesExcluding dir []

findHsFilesExcluding :: FilePath -> [FilePath] -> IO [FilePath]
findHsFilesExcluding dir excludeDirs = do
  exists <- doesDirectoryExist dir
  if not exists then pure [] else go dir
  where
    excludeNorms = map normalise excludeDirs

    go d = do
      entries <- listDirectory d
      fmap concat $ forM entries $ \entry -> do
        let full = d </> entry
        isDir <- doesDirectoryExist full
        if isDir
          then if normalise full `elem` excludeNorms
               then pure []
               else go full
          else pure [full | ".hs" `isSuffixOf` entry]

-- | Spec for globbing a source directory
data GlobSpec = GlobSpec
  { gsSrcDir       :: FilePath
  , gsExcludeDirs  :: [FilePath]  -- subdirs that are also source dirs
  , gsExcludeFiles :: [FilePath]  -- specific files to skip (e.g. exe main)
  }

-- | Discover modules from a package.yaml file.
-- Uses explicit module lists when available, falls back to globbing source dirs.
discoverHpackModules :: HpackDiscoverOpts -> FilePath -> IO [Module]
discoverHpackModules HpackDiscoverOpts{..} yamlFile = do
  pkg <- either (fail . show) pure =<< decodeFileEither yamlFile
  let (dirsToGlob, explicitModules) = partitionEithers $ mconcat
        [ discoverLibrary Nothing (pyLibrary pkg)
        , discoverInternalLibraries (pyInternalLibraries pkg)
        , discoverExecutables (pyExecutables pkg)
        , discoverTests (pyTests pkg)
        ]

  -- Validate explicit modules (check files exist)
  validated <- catMaybes <$> pooledMapConcurrentlyN hpackNumJobs validateModule explicitModules

  -- Glob source directories for .hs files
  globbed <- fmap concat $ pooledMapConcurrentlyN hpackNumJobs globSourceDir dirsToGlob

  pure $ validated ++ globbed

  where
    shouldDiscover :: CabalUnit -> Bool
    shouldDiscover unit = Discovered == foldMap1 (`discoversUnit` unit) hpackToDiscover

    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (\x (ls, rs) -> case x of Left l -> (l:ls, rs); Right r -> (ls, r:rs)) ([], [])

    validateModule :: Module -> IO (Maybe Module)
    validateModule m@Module{path} = case path of
      ModuleFile fp -> do
        exists <- doesFileExist fp
        pure $ if exists then Just m else Nothing
      ModuleNoFile -> pure $ Just m

    globSourceDir :: GlobSpec -> IO [Module]
    globSourceDir GlobSpec{..} = do
      hsFiles <- findHsFilesExcluding gsSrcDir gsExcludeDirs
      let excludeNorms = map normalise gsExcludeFiles
      pure [ Module { name = pathToModuleName gsSrcDir f, path = ModuleFile f }
           | f <- hsFiles
           , normalise f `notElem` excludeNorms
           ]

    -- | Compute exclude dirs: for each source dir, exclude other source dirs
    -- that are proper subdirectories of it.
    mkGlobSpecs :: [FilePath] -> [FilePath] -> [GlobSpec]
    mkGlobSpecs excludeFiles srcDirs =
        [ GlobSpec
            { gsSrcDir = sd
            , gsExcludeDirs = filter (isProperSubdirOf sd) srcDirs
            , gsExcludeFiles = excludeFiles
            }
        | sd <- srcDirs
        ]

    isProperSubdirOf :: FilePath -> FilePath -> Bool
    isProperSubdirOf parent child =
        let p = normalise parent ++ "/"
            c = normalise child
        in c /= normalise parent && p `isPrefixOf` c

    discoverLibrary :: Maybe String -> Maybe ComponentConfig -> [Either GlobSpec Module]
    discoverLibrary _ Nothing = []
    discoverLibrary libName (Just ComponentConfig{..})
      | not (shouldDiscover (CabalLibraryUnit libName)) = []
      | Just exposed <- ccExposedModules =
          let others = fromMaybe [] ccOtherModules
          in concatMap (fmap Right . modulesFromExplicit ccSourceDirs) (exposed ++ others)
      | otherwise = map Left $ mkGlobSpecs [] ccSourceDirs

    discoverInternalLibraries :: Map String ComponentConfig -> [Either GlobSpec Module]
    discoverInternalLibraries = Map.foldMapWithKey $ \n cfg ->
        discoverLibrary (Just n) (Just cfg)

    discoverExecutables :: Map String ExecutableConfig -> [Either GlobSpec Module]
    discoverExecutables = Map.foldMapWithKey $ \n ExecutableConfig{..} ->
        if not (shouldDiscover (CabalExecutableUnit n)) then [] else
        let mainFile = case ecMain of
              Just mf -> mf
              Nothing -> "Main.hs"
            mainPath = head ecSourceDirs </> mainFile
            mainMod = Right Module
              { name = fromString $ n ++ "-Main"
              , path = ModuleFile mainPath
              }
            otherMods = case ecOtherModules of
              Just others -> concatMap (fmap Right . modulesFromExplicit ecSourceDirs) others
              Nothing     -> map Left $ mkGlobSpecs [mainPath] ecSourceDirs
        in mainMod : otherMods

    discoverTests :: Map String ExecutableConfig -> [Either GlobSpec Module]
    discoverTests = Map.foldMapWithKey $ \n ExecutableConfig{..} ->
        if not (shouldDiscover (CabalTestsUnit n)) then [] else
        let mainFile = case ecMain of
              Just mf -> mf
              Nothing -> "Main.hs"
            mainPath = head ecSourceDirs </> mainFile
        in case ecOtherModules of
          Just others -> concatMap (fmap Right . modulesFromExplicit ecSourceDirs) others
          Nothing     -> map Left $ mkGlobSpecs [mainPath] ecSourceDirs

    modulesFromExplicit :: [FilePath] -> String -> [Module]
    modulesFromExplicit srcDirs modName
      | "Paths_" `T.isPrefixOf` T.pack modName =
          [Module { name = fromString modName, path = ModuleNoFile }]
      | otherwise =
          [ Module
            { name = fromString modName
            , path = ModuleFile $ sd </> map (\c -> if c == '.' then '/' else c) modName ++ ".hs"
            }
          | sd <- srcDirs
          ]

discoverHpackModuleGraph :: HpackDiscoverOpts -> FilePath -> IO CabalGraph
discoverHpackModuleGraph opts@HpackDiscoverOpts{..} yamlFile = do
  mods <- discoverHpackModules opts yamlFile
  buildModuleGraph hpackNumJobs hpackIncludeExternal hpackPruneTo mods
