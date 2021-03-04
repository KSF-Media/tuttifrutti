{-# OPTIONS_GHC -Wno-orphans           #-}
module Tuttifrutti.Prelude
  ( module X
  , module Tuttifrutti.Prelude
  )
  where

import           RIO                         as X hiding (Category, Handle,
                                                   Handler, LogFunc, first,
                                                   logError, logInfo, logWarn,
                                                   over, second, set, view,
                                                   (.~), (<>), (^.), (^?))

import           Control.Applicative         as X (Alternative, empty, (<|>))
import           Control.Error               as X (hush)
import           Control.Lens                as X (set, view, (.~), (<&>), (?~),
                                                   (^.), (^?), _Just)
import           Control.Monad               as X (guard, join, mfilter, mzero,
                                                   replicateM, unless, void,
                                                   when, (<=<), (>=>))
import           Control.Monad.Catch         as X (MonadMask)
import           Control.Monad.Except        as X (ExceptT (..), MonadError,
                                                   catchError, runExceptT,
                                                   throwError)
import           Control.Monad.Identity      as X (Identity (..))
import           Control.Monad.IO.Class      as X (MonadIO (..), liftIO)
import           Control.Monad.Reader        as X (MonadReader (..), Reader,
                                                   ReaderT (..), asks,
                                                   mapReaderT, runReader,
                                                   runReaderT, withReaderT)
import           Control.Monad.Trans         as X (MonadTrans, lift)
import           Control.Monad.Trans.Maybe   as X (MaybeT (..), exceptToMaybeT,
                                                   maybeToExceptT)
import           Data.Aeson                  as X (FromJSON (..), ToJSON (..),
                                                   object, (.=))
import           Data.Align                  as X (align, alignWith)
import           Data.Bifunctor              as X (bimap, first, second)
import           Data.Data                   as X (Data)
import           Data.Either                 as X (isLeft, isRight)
import           Data.Foldable               as X (asum, fold, forM_, for_,
                                                   sequence_, toList, traverse_)
import           Data.Function               as X (on, (&))
import           Data.Functor.Contravariant  as X (Contravariant (..))
import           Data.Has                    as X (Has)
import           Data.Kind                   as X (Type)
import           Data.List                   as X (find, sort)
import           Data.List.NonEmpty          as X (NonEmpty (..))
import           Data.Maybe                  as X (catMaybes, fromMaybe, isJust,
                                                   isNothing, maybe)
import           Data.Monoid                 as X (Any (..), Endo (..),
                                                   Sum (..), (<>))
import           Data.Proxy                  as X (Proxy (Proxy))
import           Data.Semigroup              as X (Semigroup, sconcat)
import           Data.String                 as X (IsString (..))
import qualified Data.Text                   as Text (replace)
import           Data.These                  as X (These (..))
import           Data.These.Lens             as X (_That, _These, _This)
import           Data.Traversable            as X (for, forM)
import           Data.Typeable               as X (TypeRep, Typeable, typeOf,
                                                   typeRep)
import           Data.UUID                   as X (UUID)
import           Data.Void                   as X (Void, absurd)
import           Database.Persist.Postgresql as X (PersistField,
                                                   PersistFieldSql,
                                                   PersistValue (..),
                                                   SqlType (..),
                                                   fromPersistValue, sqlType,
                                                   toPersistValue)
import           GHC.Exts                    as X (fromList)
import           GHC.Generics                as X (Generic)
import           GHC.OverloadedLabels        as X (IsLabel (fromLabel))
import           GHC.TypeLits                as X (KnownSymbol, Symbol,
                                                   symbolVal)
import           RIO.ByteString              as X (ByteString)
import           RIO.Map                     as X (Map)
import           RIO.Set                     as X (Set)
import           RIO.Text                    as X (decodeUtf8With, encodeUtf8,
                                                   lenientDecode)
import           RIO.Text                    as X (Text)
import           RIO.Time                    as X (Day, UTCTime, ZonedTime)
import           Text.Read                   as X (readEither, readMaybe)
import           UnliftIO                    as X (MonadUnliftIO)
import           UnliftIO.Exception          as X (Exception, bracket, catch,
                                                   catchAny, finally, handle,
                                                   onException, throwIO,
                                                   throwString, try)
import           Web.HttpApiData             as X (FromHttpApiData,
                                                   ToHttpApiData)
import           Web.PathPieces              as X (PathPiece (..))


import qualified Data.UUID                   as UUID


with :: env -> ReaderT env m a -> m a
with = flip runReaderT


fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM m = maybe m pure

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing = flip fromMaybeM

onLeft :: Applicative m => (l -> m r) -> Either l r -> m r
onLeft f = either f pure

-- | Execute action and throw exception if the result is 'Left'
throwLeft :: (Exception e, MonadThrow m) => m (Either e b) -> m b
throwLeft m = m >>= onLeft throwM

-- Note: orphan instances copied straight from Persistent documentation
-- Note: we're taking advantage of PostgreSQL understanding UUID values,
-- Note: Persistent is deprecating PersistDbSpecific:
-- `Deprecated: Deprecated since 2.11 because of inconsistent escaping behavior across backends. The Postgres backend escapes these values, while the MySQL backend does not. If you are using this, please switch to PersistLiteral or PersistLiteralEscaped based on your needs.`
instance PersistField UUID where
  toPersistValue = PersistLiteralEscaped . UUID.toASCIIBytes
  fromPersistValue (PersistLiteral uuid) =
    case UUID.fromASCIIBytes uuid of
      Nothing -> Left $ "Tuttifrutti.DB: Failed to deserialize a UUID; received: " <> tshow uuid
      Just uuid' -> Right uuid'
  fromPersistValue (PersistLiteralEscaped uuid) =
    case UUID.fromText $ Text.replace "\"" "" $ decodeUtf8Lenient uuid of
      Nothing -> Left $ "Tuttifrutti.DB: Failed to deserialize a UUID; received: PersistLiteralEscaped " <> tshow uuid
      Just uuid' -> Right uuid'
  fromPersistValue x = Left $ "Tuttifrutti.DB: When trying to deserialize a UUID: expected PersistLiteralEscaped or PersistLiteral, received: " <> tshow x

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  fromPathPiece = UUID.fromText
  toPathPiece = UUID.toText
