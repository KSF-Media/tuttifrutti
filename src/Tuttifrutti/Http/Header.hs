{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
module Tuttifrutti.Http.Header where

import Tuttifrutti.Prelude

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Char8 as ByteString8
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Swagger   (ToParamSchema(..))
import           Data.Bifunctor (first)
import           Web.HttpApiData

-- | @Cache-Control@ header contains a list of directives identified case-insentively
--   and containing optional arguments
newtype CacheControl = CacheControl
  { cacheControlDirectives :: [(CI ByteString, Maybe ByteString)] }
  deriving (Show, Read, Eq, Ord, Generic, Typeable)

pattern MaxAge :: CI ByteString
pattern MaxAge = "max-age"

pattern MaxAge0 :: (CI ByteString, Maybe ByteString)
pattern MaxAge0 = ("max-age", Just "0")

instance FromHttpApiData CacheControl where
  parseHeader = first Text.pack . Atto.parseOnly pCacheControl
  parseQueryParam = first Text.pack . Atto.parseOnly pCacheControl . Text.encodeUtf8

instance ToParamSchema CacheControl where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

-- | Parser for 'Cache-Control' header. Follows the
--   <https://tools.ietf.org/html/rfc7234#section-5.2 RFC7234>
--
--       Cache-Control   = 1#cache-directive
--       cache-directive = token [ "=" ( token / quoted-string ) ]
--
--   >>> Atto.parseOnly pCacheControl "max-age=0"
--   Right (CacheControl {cacheControlDirectives = [("max-age",Just "0")]})
--   >>> Atto.parseOnly pCacheControl "max-age=0, private, community=\"UCI\""
--   Right (CacheControl {cacheControlDirectives = [("max-age",Just "0"),("private",Nothing),("community",Just "UCI")]})
pCacheControl :: Atto.Parser CacheControl
pCacheControl =
  CacheControl <$> pList1 do
    key <- pToken
    value <- do
      optional (Atto.char '=') >>= traverse \_ ->
        pQuotedString <|> pToken
    pure (CI.mk key, value)

-- | Parser for a token as defined in
--   <https://tools.ietf.org/html/rfc7230#section-3.2.6 RFC7230>.
pToken :: Atto.Parser ByteString
pToken = Atto.takeWhile1 isTokenChar Atto.<?> "pToken"

isTokenChar :: Char -> Bool
isTokenChar c = or
  [ c `elem` ("!#$%&'*+-.^_`|~" :: String)
  , Char.isDigit c
  , Char.isAlpha c
  ]

-- | Parser for a quoted string as defined in
--   <https://tools.ietf.org/html/rfc7230#section-3.2.6 RFC7230>.
pQuotedString :: Atto.Parser ByteString
pQuotedString = do
  void $ Atto.char '"'
  string <- quotedText Atto.<?> "quotedText"
  void $ Atto.char '"'
  pure string
  where
    quotedText = fold <$> many do
        quotedTextToken <|> fmap ByteString8.singleton quotedPair
      where
        quotedTextToken = Atto.takeWhile1 \c ->
          c /= '"' &&
          or [ c == '\t'
             , c == ' '
             , Char.ord c >= 0x21 && Char.ord c <= 0x27
             , Char.ord c >= 0x2A && Char.ord c <= 0x5B
             , Char.ord c >= 0x5D && Char.ord c <= 0x7E
             , isObsChar c
             ]
        isObsChar c = Char.ord c >= 0x80 && Char.ord c <= 0xFF
        quotedPair = Atto.char '\\' *> Atto.satisfy \c -> or
          [ c == '\t'
          , c == ' '
          , Char.ord c >= 0x21 && Char.ord c <= 0x7E
          , isObsChar c
          ]



-- | Parser for an "ABNF list Extension" (so-called 1#rule)
---  as defined in <https://tools.ietf.org/html/rfc7230#section-7 RFC7230>,
--   but without ability to restrict number of elements.
--
--   >>> Atto.parseOnly (pList Atto.decimal) "1, 2, 3, 4"
--   Right [1,2,3,4]
--   >>> Atto.parseOnly (pList Atto.decimal) "1 ,2,3,  4"
--   Right [1,2,3,4]
pList1 :: Atto.Parser a -> Atto.Parser [a]
pList1 pElement = pElement `Atto.sepBy1` (skipOWS <* Atto.char ',' *> skipOWS)

-- | Consumes optional whitespace as defined in
--   <https://tools.ietf.org/html/rfc7230#section-3.2.3 RFC7230>.
skipOWS :: Atto.Parser ()
skipOWS = Atto.skipWhile (\c -> c == ' ' || c == '\t')

-- | Consumes required whitespace as defined ni
--   <https://tools.ietf.org/html/rfc7230#section-3.2.3 RFC7230>.
skipRWS :: Atto.Parser ()
skipRWS = void $ Atto.takeWhile1 (\c -> c == ' ' || c == '\t')
