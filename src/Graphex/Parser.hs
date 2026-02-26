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
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.IO    as TLIO
import           Data.Void
import           System.IO            (withFile, IOMode(ReadMode))

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
-- Splits into lines, strips block comments, takes only the file header
-- (up to the first line of code), then parses the import lines.
-- With lazy IO, only the top of the file is read from disk.
extractImports :: TL.Text -> [Import]
extractImports = mapMaybe (parseImportLine . TL.toStrict)
               . filter isImportLine
               . takeWhile isHeaderLine
               . stripBlockComments
               . TL.lines

-- | Remove lines that fall inside block comments.
stripBlockComments :: [TL.Text] -> [TL.Text]
stripBlockComments = go False
    where
        go _ [] = []
        go True (l:ls)
            | TL.isInfixOf "-}" l = go False ls
            | otherwise           = go True ls
        go False (l:ls)
            | isOpenComment l = go (not $ TL.isInfixOf "-}" l) ls
            | otherwise       = l : go False ls

        isOpenComment l = let s = TL.stripStart l
                          in TL.isPrefixOf "{-" s && not (TL.isPrefixOf "{-#" s)

-- | Lines that can appear in the file header before code begins.
-- Includes indented lines (spaces/tabs) which are import list continuations.
isHeaderLine :: TL.Text -> Bool
isHeaderLine l =
    isImportLine l
    || TL.null (TL.strip l)
    || TL.isPrefixOf " "      l
    || TL.isPrefixOf "\t"     l
    || TL.isPrefixOf "--"     s
    || TL.isPrefixOf "{-#"    s
    || TL.isPrefixOf "module " l
    || TL.isPrefixOf "#"      s
    where s = TL.stripStart l

isImportLine :: TL.Text -> Bool
isImportLine = TL.isPrefixOf "import "

parseFileImports :: FilePath -> IO [Import]
parseFileImports fp =
  withFile fp ReadMode $ \h -> do
    contents <- TLIO.hGetContents h
    let !imports = extractImports contents
    pure imports
