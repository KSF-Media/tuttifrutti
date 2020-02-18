{-# LANGUAGE OverloadedStrings #-}

<<<<<<< HEAD
module Tuttifrutti.Template
  (render
  ) where

import Prelude

import qualified Data.Text.Lazy     as S
import qualified Data.Text          as T
import           Data.Maybe

import qualified Data.Text.Template as Template

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Template.Context
=======
module Tuttifrutti.Template where

import Prelude

import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E
import           Data.Maybe

import Data.Text.Template

-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
>>>>>>> Add Template module
context assocs x = fromMaybe err . lookup x $ assocs
  where
    err = error $ "Could not find key: " ++ T.unpack x

<<<<<<< HEAD
render :: T.Text -> [(T.Text, T.Text)] -> T.Text
render template' context' = S.toStrict $ Template.substitute template' context''
=======
render :: T.Text -> [(T.Text, T.Text)] -> S.ByteString
render template' context' = E.encodeUtf8 $ substitute template' context''
>>>>>>> Add Template module
  where
    context''  = context context'
