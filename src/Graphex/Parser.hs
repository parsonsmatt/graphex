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

import           Data.Maybe           (mapMaybe)
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Void

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

-- | Extract imports from strict text using line-based scanning.
--
-- Filters to lines starting with "import " and parses each one.
-- Block comments are stripped first to avoid false positives.
extractImports :: Text -> [Import]
extractImports = mapMaybe parseImportLine
               . filter isImportLine
               . stripBlockComments
               . T.lines

-- | Remove lines that fall inside block comments.
stripBlockComments :: [Text] -> [Text]
stripBlockComments = go False
    where
        go _ [] = []
        go True (l:ls)
            | T.isInfixOf "-}" l = go False ls
            | otherwise          = go True ls
        go False (l:ls)
            | isOpenComment l = go (not $ T.isInfixOf "-}" l) ls
            | otherwise       = l : go False ls

        isOpenComment l = let s = T.stripStart l
                          in T.isPrefixOf "{-" s && not (T.isPrefixOf "{-#" s)

isImportLine :: Text -> Bool
isImportLine = T.isPrefixOf "import "

parseFileImports :: FilePath -> IO [Import]
parseFileImports fp = extractImports <$> TIO.readFile fp
