module Tuttifrutti.TestPrelude
  ( module X
  , module Tuttifrutti.Prelude
  , module Tuttifrutti.TestPrelude
  , Tasty.ResourceSpec(..)
  , module Test.Tasty.ExpectedFailure
  ) where

import           Tuttifrutti.Prelude

import qualified Data.Aeson                     as Json
import           Data.Aeson.Diff                as Json
import           Data.Aeson.Encode.Pretty       as Json
import qualified Data.ByteString.Lazy           as LByteString
import qualified Data.Has                       as Has
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Vcr                       as Vcr
import           Servant.Server                 (ServerError (..))

import qualified Tuttifrutti.Http               as Http
import qualified Tuttifrutti.Http.Handle        as Http
import           Tuttifrutti.Log                as X (MonadLog)
import qualified Tuttifrutti.Time.Handle        as Time

import           Test.Hspec.Expectations.Lifted as X
import           Test.Tasty                     as X (TestName, TestTree,
                                                      testGroup)
import qualified Test.Tasty                     as Tasty
import           Test.Tasty.ExpectedFailure     (expectFail, expectFailBecause,
                                                 ignoreTest, ignoreTestBecause)
import qualified Test.Tasty.Golden.Advanced     as Tasty
import           Test.Tasty.HUnit               as Tasty.HUnit
import qualified Test.Tasty.Ingredients.Vcr     as Vcr
import qualified Test.Tasty.Runners             as Tasty

newtype Spec env = Spec
  { unSpec :: IO env -> TestTree }

instance Contravariant Spec where
  contramap f (Spec s) = Spec $ \getEnv -> s $ fmap f getEnv
-- | Apply a function to a 'testTree' inside the 'Spec'.
--
--   @mapTests expectFail $ test "my fail" $ error "fail"@
mapTests :: (TestTree -> TestTree) -> Spec env -> Spec env
mapTests f (Spec s) = Spec (f . s)

spec :: TestName -> [Spec env] -> Spec env
spec name specs = Spec
  (\getEnv -> testGroup name $ traverse unSpec specs getEnv)

test :: TestName -> RIO env a -> Spec env
test name assertion = Spec
  (\getEnv -> Tasty.HUnit.testCase name $ void $ do
      env <- getEnv
      runRIO env assertion)

testSpec :: Tasty.ResourceSpec env -> Spec env -> TestTree
testSpec (Tasty.ResourceSpec acquire dispose) (Spec m) =
  Tasty.withResource acquire dispose m

specWithResource :: IO a -> (a -> IO ()) -> (IO a -> Spec env) -> Spec env
specWithResource create destroy f =
  Spec $ \getEnv -> Tasty.withResource create destroy $ \getA -> unSpec (f getA) getEnv

askVcrEnabled :: (Bool -> Spec env) -> Spec env
askVcrEnabled f =
  Spec
    $ \getEnv -> Tasty.askOption
    $ \(Vcr.EnabledFlag vcrEnabled) ->
         unSpec (f vcrEnabled) getEnv

-- | Modifies the 'Spec' to use the cassette at given path (only if --vcr option is enabled).
specVcrCassette :: (Has Http.Handle env) => FilePath -> Spec env -> Spec env
specVcrCassette path (Spec s) =
  Spec
    $ \getEnv -> Tasty.askOption
    $ \(Vcr.EnabledFlag vcrEnabled) ->
        if vcrEnabled
           then Tasty.withResource
                  (Vcr.createRecorder >>= Vcr.loadRecorder path)
                  (Vcr.saveRecorder path)
                  (\getRecorder ->
                     tagTreeLeafs "with VCR" $ s $ do
                       Http.useVcrRecorder <$> getRecorder <*> getEnv)
           else s getEnv

-- | Traverse a 'TestTree' and append given tag to every leaf 'TestName'.
tagTreeLeafs :: String -> TestTree -> TestTree
tagTreeLeafs tag = \case
  Tasty.SingleTest name someTest ->
    Tasty.SingleTest (name <> " " <> "[" <> tag <> "]") someTest
  Tasty.TestGroup name someTests ->
    Tasty.TestGroup name $ map (tagTreeLeafs tag) someTests
  whatever -> whatever

-- | Traverse a 'Spec' and append given tag to every leaf 'TestName'.
tagSpecLeafs :: String -> Spec env -> Spec env
tagSpecLeafs = mapTests . tagTreeLeafs

-- | Traveres a 'Spec' and tag all tests as @[pure]@.
--   Should be applied to tests that do no side effects.
tagPureSpec :: Spec env -> Spec env
tagPureSpec = tagSpecLeafs "pure"

-- | Modifies the 'Spec' so that the time is frozen at given instant (only if --vcr option is enabled).
specVcrFrozenTime :: (Has Time.Handle env) => UTCTime -> Spec env -> Spec env
specVcrFrozenTime =
  contramap . Has.modifier . const . runIdentity . Time.newFrozenTime

goldenTest
  :: (ToJSON a, FromJSON a)
  => TestName -> FilePath -> RIO env a -> Spec env
goldenTest name path action = Spec
  (\getEnv -> do
      let getActualValue = do
            env <- getEnv
            runRIO env action
      Tasty.goldenTest
        name
        getCorrectValue getActualValue
        compareValues updateCorrectValue)
  where
    getCorrectValue = do
      LByteString.readFile path
        >>= onLeft (error . ((path <> ": ") <>)) . Json.eitherDecode
    compareValues a b =
      pure $ case Json.diff (toJSON a) (toJSON b) of
        Json.Patch [] -> Nothing
        changes       ->
          Just $ Text.unpack $ Text.decodeUtf8 $ LByteString.toStrict $ Json.encodePretty changes
    updateCorrectValue = LByteString.writeFile path . Json.encodePretty

-- | Test expectation that passes when a given action throws an ServantError with given HTTP code
throwsServantError :: (HasCallStack, MonadUnliftIO m) => m a -> Int -> m ()
action `throwsServantError` httpCode = do
  try action >>= \case
    Right _ -> expectationFailure $ "did not get the expected ServantError with code " <> show httpCode
    Left (ServerError{ errHTTPCode }) | errHTTPCode == httpCode -> pure ()
    Left err -> expectationFailure
      $ "did not get the expected ServantError with code "
      <> show httpCode
      <> ", but got this instead: "
      <> show err
