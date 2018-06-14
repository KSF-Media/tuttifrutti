module Tuttifrutti.Prelude
  ( module X 
  , module Tuttifrutti.Prelude
  ) 
  where

import           Control.Applicative        as X (Alternative, empty, (<|>))
import           Control.Error              as X (hush)
import           Control.Monad              as X (guard, join, mfilter, mzero, replicateM, unless,
                                                  void, when, (<=<), (>=>))
import           Control.Monad.Catch        as X (MonadCatch, MonadMask (..), MonadThrow (..))
import           Control.Monad.Except       as X (ExceptT (..), MonadError, catchError, runExceptT,
                                                  throwError)
import           Control.Monad.Identity     as X (Identity (..))
import           Control.Monad.IO.Class     as X (MonadIO (..), liftIO)
import           Control.Monad.Reader       as X (MonadReader (..), Reader, ReaderT (..), asks,
                                                  mapReaderT, runReader, runReaderT, withReaderT)
import           Control.Monad.Trans        as X (MonadTrans, lift)
import           Control.Monad.Trans.Maybe  as X (MaybeT (..), exceptToMaybeT, maybeToExceptT)
import           CorePrelude                as X hiding (first, second)
import           Data.Aeson                 as X (FromJSON (..), ToJSON (..), object, (.=))
import           Data.Align                 as X (align, alignWith)
import           Data.Bifunctor             as X (bimap, first, second)
import           Data.Data                  as X (Data)
import           Data.Either                as X (isLeft, isRight)
import           Data.Foldable              as X (asum, fold, forM_, for_, sequence_, toList,
                                                  traverse_)
import           Data.Function              as X (on, (&))
import           Data.Functor.Contravariant as X (Contravariant (..))
import           Data.Has                   as X (Has)
import           Data.Kind                  as X (Type)
import           Data.List                  as X (find, sort)
import           Data.List.NonEmpty         as X (NonEmpty (..))
import           Data.Maybe                 as X (catMaybes, fromMaybe, isJust, isNothing, maybe)
import           Data.Monoid                as X (Any (..), Endo (..), Sum (..), (<>))
import           Data.Proxy                 as X (Proxy (Proxy))
import           Data.Semigroup             as X (Semigroup, sconcat)
import           Data.String                as X (IsString (..))
import           Data.These                 as X (These (..), _That, _These, _This)
import           Data.Traversable           as X (for, forM)
import           Data.Typeable              as X (TypeRep, Typeable, typeOf, typeRep)
import           Data.Void                  as X (Void, absurd)
import           GHC.Exts                   as X (fromList)
import           GHC.Generics               as X (Generic)
import           GHC.OverloadedLabels       as X (IsLabel (fromLabel))
import           GHC.TypeLits               as X (KnownSymbol, Symbol, symbolVal)
import           RIO                        as X (Hashable, lookup)
import           RIO.ByteString             as X (ByteString)
import           RIO.Map                    as X (Map)
import           RIO.Set                    as X (Set)
import           RIO.Text                   as X (decodeUtf8With, encodeUtf8, lenientDecode)
import           RIO.Text                   as X (Text)
import           RIO.Time                   as X (Day, UTCTime, ZonedTime)
import           Text.Read                  as X (readEither, readMaybe)
import           UnliftIO                   as X (MonadUnliftIO)
import           UnliftIO.Exception         as X (Exception, bracket, catch, catchAny, finally,
                                                  handle, onException, throwIO, throwString, try)
import           Web.HttpApiData            as X (FromHttpApiData, ToHttpApiData)

with :: env -> ReaderT env m a -> m a
with = flip runReaderT

