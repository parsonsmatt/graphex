{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

-- | Implements a very crude Haskell import parser.
--
-- All it does is parse full module names that are imported in a file,
-- ignoring false positives.
--
-- It does not use CPP, so all conditional imports are parsed out. For
-- graphex, this makes sense in a way - those are all still dependencies.
module Graphex.Parser where

import           Control.Monad                  (filterM)
import qualified Control.Monad.Trans.State.Lazy as SL
import           Data.Char                      (isLower)
import           Data.Maybe                     (listToMaybe, mapMaybe)
import qualified Data.Set                       as Set
import           Data.String                    (IsString)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as TL
import qualified Data.Text.Lazy.IO              as TLIO
import           Data.Void
import           System.IO                      (IOMode (ReadMode), withFile)

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Graphex.Core

-- | Parse a single import statement from a line known to start with "import".
importParser
  :: MonadParsec e s m
  => Token s ~ Char
  => IsString (Tokens s)
  => m Import
importParser = do
  _ <- string "import"
  space1
  -- Handle prefix qualified imports
  _ <- optional (string "qualified")
  space
  -- Handle PackageImports
  pkg <- optional $ between (char '"') (char '"') $ some $ alphaNumChar <|> char '-' <|> char '_'
  space

  modid <- some $ alphaNumChar <|> char '.' <|> char '_'

  pure Import
    { module_ = ModuleName $ T.pack modid
    , package = T.pack <$> pkg
    }

-- | Try to parse a strict Text line as an import.
parseImportLine :: Text -> Maybe Import
parseImportLine line =
  case runParser (importParser @Void @Text) "" line of
    Left _  -> Nothing
    Right x -> Just x

-- | Extract imports from lazy text using line-based scanning.
--
-- Strips block comments, stops at the first line that looks like
-- a top-level declaration, then parses the import lines.
-- With lazy IO, only the file header is read from disk.
extractImports :: TL.Text -> [Import]
extractImports = mapMaybe (parseImportLine . TL.toStrict)
               . filter isImportLine
               . takeWhile (not . isCodeLine)
               . stripBlockComments
               . TL.lines

stripBlockComments :: [TL.Text] -> [TL.Text]
stripBlockComments = flip SL.evalState False . filterM step
  where
    step line = do
      let stripped = TL.stripStart line
      let commentStarts = TL.isPrefixOf "{-" stripped && not (TL.isPrefixOf "{-#" stripped)
      let commentEnds = TL.isInfixOf "-}" line
      wasInComment <- SL.get
      let inComment = wasInComment || commentStarts
      let stillInComment = inComment && not commentEnds
      SL.put stillInComment
      pure (not inComment)

isImportLine :: TL.Text -> Bool
isImportLine = TL.isPrefixOf "import "

-- | Top-level declaration keywords that signal the end of the import section.
codeKeywords :: Set.Set TL.Text
codeKeywords = Set.fromList
    [ "data", "type", "newtype", "class", "instance"
    , "deriving", "pattern", "foreign", "default"
    , "infixl", "infixr", "infix"
    ]

-- | Detect lines that are definitely top-level code declarations,
-- signaling the end of the import section.
isCodeLine :: TL.Text -> Bool
isCodeLine line =
    maybe False (\firstWord -> firstWord `Set.member` codeKeywords && firstWord `TL.isPrefixOf` line) (listToMaybe $ TL.words line)
    || isFunctionSig line

-- | Detect top-level function signatures like @foo :: Type@.
-- Matches lines where a lowercase identifier is followed by @::@.
isFunctionSig :: TL.Text -> Bool
isFunctionSig line = case TL.uncons line of
    Just (ch, _) | isLower ch || ch == '_' -> TL.isInfixOf "::" line
    _                                      -> False

parseFileImports :: FilePath -> IO [Import]
parseFileImports fp =
    withFile fp ReadMode $ \h -> do
        contents <- TLIO.hGetContents h
        let imports = extractImports contents
        length imports `seq` pure imports
