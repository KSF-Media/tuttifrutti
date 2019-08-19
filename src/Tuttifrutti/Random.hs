module Tuttifrutti.Random where

import           Tuttifrutti.Prelude

import qualified Data.Set            as Set
import qualified Data.Text           as Text
import           System.Random

randomAlphaNum :: MonadIO m => Int -> m Text
randomAlphaNum size = do
  gen <- liftIO System.Random.newStdGen
  pure $ Text.pack $ take size $ randomStreamOf alphaNum gen
  where
    alphaNum = Set.fromList (['a' .. 'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- | Given a set of values generates a random infinite stream with set members.
randomStreamOf :: RandomGen g => Set a -> g -> [a]
randomStreamOf elems gen = (flip Set.elemAt) elems <$> stream
  where
    range = (0, (length elems) - 1)
    stream = randomRs range gen
