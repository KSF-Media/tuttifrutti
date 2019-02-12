module Tuttifrutti.Cache.Tests where

import           Prelude
import           Tuttifrutti.Prelude

import qualified Data.Time                          as Time

import           Test.Hspec.Expectations.Lifted     as Hspec
import           Test.Tasty                         as Tasty
import           Test.Tasty.HUnit                   as HUnit

import qualified Tuttifrutti.Cache                  as Cache
import qualified Tuttifrutti.Cache.Storage          as Cache.Storage
import qualified Tuttifrutti.Cache.Storage.InMemory as Cache.InMemory

tests :: Tasty.TestTree
tests = Tasty.testGroup "TuttiFrutti.Cache"
  [
  ]
