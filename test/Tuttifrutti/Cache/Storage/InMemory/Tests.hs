module Tuttifrutti.Cache.Storage.InMemory.Tests where

import           Data.Time

import           Test.Hspec.Expectations.Lifted     as Hspec
import           Test.Tasty                         as Tasty
import           Test.Tasty.HUnit                   as HUnit

import qualified Tuttifrutti.Cache.Storage          as Storage
import qualified Tuttifrutti.Cache.Storage.InMemory as InMemory
import           Tuttifrutti.Prelude

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "TuttiFrutti.Cache.Storage.InMemory"
  [ HUnit.testCase "Insert many values and lookup the most recent" $ do
      storage :: Storage.Handle Int UTCTime Int STM <- atomically $ InMemory.newHandle 1024
      let insertN n = do
            now <- getCurrentTime
            atomically $ Storage.insert storage (n, now, n)
      for_ [1..1000] insertN
      cached <- atomically $ for [1..1000] $ Storage.lookup storage
      -- older values should get evicted once capacity is reached
      catMaybes cached `shouldNotContain` [1..5]
      -- newer values should however be there
      catMaybes cached `shouldContain` [995..1000]
  , HUnit.testCase "Insert many values and conditionally lookup" $ do
      storage :: Storage.Handle Int UTCTime Int STM <- atomically $ InMemory.newHandle $ 100 * 1024
      let insertN n = do
            now <- getCurrentTime
            atomically $ Storage.insert storage (n, now, n)
      for_ [1..10] insertN
      midtime <- getCurrentTime
      for_ [11..20] insertN
      oldAndOdd <- atomically $ for [1..20] $ \n ->
        Storage.lookupValid storage n $ \t v -> v <$ do
          guard $ t < midtime
          guard $ odd v
      catMaybes oldAndOdd `shouldBe` [1,3..9]
  , HUnit.testCase "Insert many values and drop older ones" $ do
      storage :: Storage.Handle Int UTCTime Int STM <- atomically $ InMemory.newHandle $ 100 * 1024
      let insertN n = do
            now <- getCurrentTime
            atomically $ Storage.insert storage (n, now, n)
      for_ [1..10] insertN
      midtime <- getCurrentTime
      for_ [11..20] insertN
      atomically $ Storage.dropLowerThan storage midtime
      newish <- atomically $ for [5..20] $ Storage.lookup storage
      oldish <- atomically $ for [1..10] $ Storage.lookup storage
      catMaybes newish `shouldBe` [11..20]
      catMaybes oldish `shouldBe` []
  ]

