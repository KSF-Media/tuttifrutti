module Main where

import           Tuttifrutti.Prelude

import           Test.Tasty              as Tasty

import qualified Tuttifrutti.Cache.Tests

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Main"
  [ Tuttifrutti.Cache.Tests.tests ]
