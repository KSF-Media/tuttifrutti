module Tuttifrutti.Cache.Tests where

import           Prelude
import           Tuttifrutti.Prelude

import qualified Data.Time                      as Time

import           Test.Hspec.Expectations.Lifted as Hspec
import           Test.Tasty                     as Tasty
import           Test.Tasty.HUnit               as HUnit

import           Tuttifrutti.Cache              as Cache

tests :: Tasty.TestTree
tests = Tasty.testGroup "TuttiFrutti.Cache"
  [ HUnit.testCase "Insert many values and lookup the most recent" $ do
      cache :: Cache Int Int <- Cache.new 1024
      let insertN n = do
            now <- Time.getCurrentTime
            Cache.insert cache n now n
      for_ [1..1000] insertN
      cached <- for [1..1000] $ Cache.lookup cache
      -- older values should get evicted once capacity is reached
      catMaybes cached `shouldNotContain` [1..5]
      -- newer values should however be there
      catMaybes cached `shouldContain` [995..1000]
  , HUnit.testCase "Insert many values and conditionally lookup" $ do
      cache :: Cache Int Int <- Cache.new $ 100 * 1024
      let insertN n = do
            now <- Time.getCurrentTime
            Cache.insert cache n now n
      for_ [1..10] insertN
      midtime <- Time.getCurrentTime
      for_ [11..20] insertN
      oldAndOdd <- for [1..20] $ \n ->
        Cache.lookupValid cache n $ \t v -> v <$ do
          guard $ t < midtime
          guard $ odd v
      catMaybes oldAndOdd `shouldBe` [1,3..9]
  , HUnit.testCase "Insert many values and drop older ones" $ do
      cache :: Cache Int Int <- Cache.new $ 100 * 1024
      let insertN n = do
            now <- Time.getCurrentTime
            Cache.insert cache n now n
      for_ [1..10] insertN
      midtime <- Time.getCurrentTime
      for_ [11..20] insertN
      newish <- for [5..20] $ \n -> Cache.lookupAfter cache n midtime
      oldish <- for [1..10] $ Cache.lookup cache
      catMaybes newish `shouldBe` [11..20]
      catMaybes oldish `shouldBe` []
  ]
