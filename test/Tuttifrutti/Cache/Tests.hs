module Tuttifrutti.Cache.Tests where

import           Prelude
import           Tuttifrutti.Prelude

import qualified Data.Time                          as Time


import           Test.Hspec.Expectations.Lifted     as Hspec
import           Test.Tasty                         as Tasty
import           Test.Tasty.HUnit                   as HUnit

import qualified Tuttifrutti.Cache                  as Cache
import qualified Tuttifrutti.Cache.Storage          as Cache.Storage
import qualified Tuttifrutti.Cache.Storage.InMemory as Storage.InMemory

tests :: Tasty.TestTree
tests = Tasty.testGroup "TuttiFrutti.Cache"
  [ cacheTests "Tuttifrutti.Cache (InMemory)" $ do
      storage <- atomically $ Storage.InMemory.newHandle $ 100 * 1024
      Cache.newHandle $ Cache.Storage.transHandle atomically storage
  ]

cacheTests :: String -> IO (Cache.Handle "cache" Int Text) -> Tasty.TestTree
cacheTests name newCache = Tasty.testGroup name
  [ HUnit.testCase "Insert and lookup some values" $ do
      cache <- newCache
      now <- Time.getCurrentTime
      with cache $ do
        Cache.insert @"cache" (1 :: Int) now ("one" :: Text)
        Cache.insert @"cache" (2 :: Int) now ("two" :: Text)
        Cache.insert @"cache" (3 :: Int) now ("three" :: Text)
        Cache.insert @"cache" (2 :: Int) now ("two+" :: Text)
      v1 <- with cache $ Cache.lookup @"cache" (1 :: Int)
      v1 `shouldBe` Just ("one" :: Text)
      v2 <- with cache $ Cache.lookup @"cache" (2 :: Int)
      v2 `shouldBe` Just ("two+" :: Text)


  ]
