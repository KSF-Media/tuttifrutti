module Main where

import qualified Data.Text               as Text
import           Tuttifrutti.Prelude

import           Test.DocTest            (doctest)
import           Test.Tasty              as Tasty

import qualified Tuttifrutti.Cache.Tests
import qualified Tuttifrutti.Package     as Package

main :: IO ()
main = do
  Tasty.defaultMain tests
  doctests

doctests :: IO ()
doctests = do
  languageExtensions <- Package.getDefaultExtensions "package.yaml"
  let extensionFlags = Text.unpack . Text.append "-X" <$> languageExtensions
      flags =
        [ "-X" <> Text.unpack extension | extension <- languageExtensions ]
          <>
        ["--verbose"]
      paths = ["src/"]
  doctest (flags <> paths)

tests :: Tasty.TestTree
tests = Tasty.testGroup "Main"
  [ Tuttifrutti.Cache.Tests.tests ]
