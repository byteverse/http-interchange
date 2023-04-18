{-# language LambdaCase #-}

module Http.Header
  ( Header(..)
  , decodeMany
  , parser
  , parserSmallArray
  , builder
  , builderSmallArray
  ) where

import Control.Monad (when)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (SmallArray,SmallMutableArray,ByteArray(ByteArray))
import Data.Word (Word8,Word16)
import Data.Text (Text)
import Data.Bytes.Builder (Builder)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Bytes.Builder as Builder
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Data.Primitive as PM

-- | An HTTP header. This type does not enforce a restricted character
-- set. If, for example, the user creates a header whose key has a colon
-- character, the resulting request will be malformed.
data Header = Header
  { name :: {-# UNPACK #-} !Text
  , value :: {-# UNPACK #-} !Text
  } deriving (Show)

uninitializedHeader :: Header
{-# noinline uninitializedHeader #-}
uninitializedHeader = errorWithoutStackTrace "parserHeaders: uninitialized header"

-- | Parse headers. Expects two CRLF sequences in a row at the end.
-- Fails if leftovers are encountered.
decodeMany :: Int -> Bytes -> Maybe (SmallArray Header)
decodeMany !n !b = Parser.parseBytesMaybe (parserSmallArray n <* Parser.endOfInput ()) b
  
-- Parse headers. Stops after encountering two CRLF sequences in
-- a row.
parserSmallArray ::
     Int -- maximum number of headers allowed, recommended 128
  -> Parser () s (SmallArray Header)
parserSmallArray !n = do
  dst <- Parser.effect (PM.newSmallArray n uninitializedHeader)
  parserHeaderStep 0 n dst

parserHeaderStep ::
     Int -- index
  -> Int -- remaining length
  -> SmallMutableArray s Header
  -> Parser () s (SmallArray Header)
parserHeaderStep !ix !n !dst = Latin.trySatisfy (== '\r') >>= \case
  True -> do
    Latin.char () '\n'
    Parser.effect $ do
      PM.shrinkSmallMutableArray dst ix
      PM.unsafeFreezeSmallArray dst
  False -> if n > 0
    then do
      header <- parser
      Parser.effect (PM.writeSmallArray dst ix header)
      parserHeaderStep (ix + 1) (n - 1) dst
    else Parser.fail ()

-- | Parse a single HTTP header including the trailing CRLF sequence.
-- From RFC 7230:
--
-- > header-field   = field-name ":" OWS field-value OWS
-- > field-name     = token
-- > field-value    = *( field-content / obs-fold )
-- > field-content  = field-vchar [ 1*( SP / HTAB ) field-vchar ]
-- > field-vchar    = VCHAR / obs-text
parser :: Parser () s Header
parser = do
  -- Header name may contain: a-z, A-Z, 0-9, underscore, hyphen
  !name <- Parser.takeWhile $ \c ->
    (c >= 0x41 && c <= 0x5A)
    ||
    (c >= 0x61 && c <= 0x7A)
    ||
    (c >= 0x30 && c <= 0x39)
    ||
    c == 0x2D
    ||
    c == 0x5F
  Latin.char () ':'
  Latin.skipWhile (\c -> c == ' ' || c == '\t')
  -- Header name allows vchar, space, and tab.
  value0 <- Parser.takeWhile $ \c ->
    (c >= 0x20 && c <= 0x7e)
    ||
    (c == 0x09)
  Latin.char2 () '\r' '\n'
  -- We only need to trim the end because the leading spaces and tab
  -- were already skipped.
  let !value = Bytes.dropWhileEnd (\c -> c == 0x20 || c == 0x09) value0
  pure Header{name=unsafeBytesToText name,value=unsafeBytesToText value}

unsafeBytesToText :: Bytes -> Text
{-# inline unsafeBytesToText #-}
unsafeBytesToText (Bytes (ByteArray arr) off len) =
  Text.Text (Data.Text.Array.ByteArray arr) off len

-- | Encode a header. Includes the trailing CRLF sequence.
builder :: Header -> Builder
builder Header{name,value} =
  Builder.copy (Utf8.fromText name)
  <>
  Builder.ascii2 ':' ' '
  <>
  Builder.copy (Utf8.fromText value)
  <>
  Builder.ascii2 '\r' '\n'

builderSmallArray :: SmallArray Header -> Builder
builderSmallArray = foldMap builder
