module ImportParserSpec where

import           Graphex.Core
import           Graphex.Parser

import           Test.Tasty
import           Test.Tasty.HUnit

unit_parseSomeFile :: IO ()
unit_parseSomeFile = do
    got <- parseFileImports "testData/parseTests/SomeFile.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "SomethingElse" Nothing,
                    Import "AnotherThing" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "This.Though" Nothing,
                    Import "Data.List" (Just "base")] got

unit_parseMultiLineImports :: IO ()
unit_parseMultiLineImports = do
    got <- parseFileImports "testData/parseTests/MultiLineImports.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.Set" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "Data.List" Nothing] got

unit_parsePostQualified :: IO ()
unit_parsePostQualified = do
    got <- parseFileImports "testData/parseTests/PostQualified.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map.Strict" Nothing,
                    Import "Data.Set" Nothing,
                    Import "Data.Maybe" Nothing,
                    Import "Data.Map" Nothing,
                    Import "Data.List" Nothing] got

unit_parseCppImports :: IO ()
unit_parseCppImports = do
    got <- parseFileImports "testData/parseTests/CppImports.hs"
    assertEqual "" [Import "Data.Foo" Nothing,
                    Import "Data.Bar" Nothing,
                    Import "Data.Baz" Nothing] got

unit_parseMultiLineModule :: IO ()
unit_parseMultiLineModule = do
    got <- parseFileImports "testData/parseTests/MultiLineModule.hs"
    assertEqual "" [Import "Data.Text" Nothing,
                    Import "Data.Map" Nothing] got

unit_parseNoImports :: IO ()
unit_parseNoImports = do
    got <- parseFileImports "testData/parseTests/NoImports.hs"
    assertEqual "" [] got
