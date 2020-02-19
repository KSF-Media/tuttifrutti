{-# LANGUAGE OverloadedStrings #-}

module Tuttifrutti.Template where

import Prelude

import qualified Data.Text.Lazy as S
import qualified Data.Text as T
import           Data.Maybe

import Data.Text.Template

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = fromMaybe err . lookup x $ assocs
  where
    err = error $ "Could not find key: " ++ T.unpack x

render :: T.Text -> [(T.Text, T.Text)] -> T.Text
render template' context' = S.toStrict $ substitute template' context''
  where
    context''  = context context'
