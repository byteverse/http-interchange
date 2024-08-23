{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Http.Header
  ( Header (..)
  , decodeMany
  , parser
  , parserSmallArray
  , builder
  , builderSmallArray
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Builder (Builder)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (ByteArray), SmallArray, SmallMutableArray)
import Data.Text (Text)
import Data.Word (Word8)

import Data.Bytes qualified as Bytes
import Data.Bytes.Builder qualified as Builder
import Data.Bytes.Parser qualified as Parser
import Data.Bytes.Parser.Latin qualified as Latin
import Data.Bytes.Text.Utf8 qualified as Utf8
import Data.Primitive qualified as PM
import Data.Text.Array qualified
import Data.Text.Internal qualified as Text

{- | An HTTP header. This type does not enforce a restricted character
set. If, for example, the user creates a header whose key has a colon
character, the resulting request will be malformed.
-}
data Header = Header
  { name :: {-# UNPACK #-} !Text
  , value :: {-# UNPACK #-} !Text
  }
  deriving (Eq, Show)

uninitializedHeader :: Header
{-# NOINLINE uninitializedHeader #-}
uninitializedHeader = errorWithoutStackTrace "parserHeaders: uninitialized header"

{- | Parse headers. Expects two CRLF sequences in a row at the end.
Fails if leftovers are encountered.
-}
decodeMany :: Int -> Bytes -> Maybe (SmallArray Header)
decodeMany !n !b = Parser.parseBytesMaybe (parserSmallArray n <* Parser.endOfInput ()) b

-- Parse headers. Stops after encountering two CRLF sequences in
-- a row.
parserSmallArray ::
  Int -> -- maximum number of headers allowed, recommended 128
  Parser () s (SmallArray Header)
parserSmallArray !n = do
  dst <- Parser.effect (PM.newSmallArray n uninitializedHeader)
  parserHeaderStep 0 n dst

parserHeaderStep ::
  Int -> -- index
  Int -> -- remaining length
  SmallMutableArray s Header ->
  Parser () s (SmallArray Header)
parserHeaderStep !ix !n !dst =
  Latin.trySatisfy (== '\r') >>= \case
    True -> do
      Latin.char () '\n'
      Parser.effect $ do
        PM.shrinkSmallMutableArray dst ix
        PM.unsafeFreezeSmallArray dst
    False ->
      if n > 0
        then do
          header <- parser
          Parser.effect (PM.writeSmallArray dst ix header)
          parserHeaderStep (ix + 1) (n - 1) dst
        else Parser.fail ()

pattern Bang :: Word8
pattern Bang = 0x21

pattern Pound :: Word8
pattern Pound = 0x23

pattern Dollar :: Word8
pattern Dollar = 0x24

pattern Percent :: Word8
pattern Percent = 0x25

pattern Ampersand :: Word8
pattern Ampersand = 0x26

pattern SingleQuote :: Word8
pattern SingleQuote = 0x27

pattern Asterisk :: Word8
pattern Asterisk = 0x2A

pattern Plus :: Word8
pattern Plus = 0x2B

pattern Hyphen :: Word8
pattern Hyphen = 0x2D

pattern Period :: Word8
pattern Period = 0x2E

pattern Caret :: Word8
pattern Caret = 0x5E

pattern Underscore :: Word8
pattern Underscore = 0x5F

pattern Backtick :: Word8
pattern Backtick = 0x60

pattern Pipe :: Word8
pattern Pipe = 0x7C

pattern Twiddle :: Word8
pattern Twiddle = 0x7E

pattern HorizontalTab :: Word8
pattern HorizontalTab = 0x09

{- | Parse a single HTTP header including the trailing CRLF sequence.
From RFC 7230:

> token          = 1*tchar
> tchar          = "!" / "#" / "$" / "%" / "&" / "'" / "*"
>                / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~" 
>                / DIGIT / ALPHA
> 
> header-field   = field-name ":" OWS field-value OWS
> field-name     = token
> field-value    = *( field-content / obs-fold )
> field-content  = field-vchar [ 1*( SP / HTAB ) field-vchar ]
> field-vchar    = VCHAR / obs-text
-}
parser :: Parser () s Header
parser = do
  -- Header name may contain: a-z, A-Z, 0-9, several different symbols
  !name <- Parser.takeWhile $ \c ->
    (c >= 0x41 && c <= 0x5A)
      || (c >= 0x61 && c <= 0x7A)
      || (c >= 0x30 && c <= 0x39)
      || c == Bang
      || c == Pound
      || c == Dollar
      || c == Percent
      || c == Ampersand
      || c == SingleQuote
      || c == Asterisk
      || c == Plus
      || c == Hyphen
      || c == Period
      || c == Caret
      || c == Underscore
      || c == Backtick
      || c == Pipe
      || c == Twiddle
  Latin.char () ':'
  Latin.skipWhile (\c -> c == ' ' || c == '\t')
  -- Header value allows vchar, space, and tab.
  value0 <- Parser.takeWhile $ \c ->
    (c >= 0x20 && c <= 0x7e)
      || (c == HorizontalTab)
  Latin.char2 () '\r' '\n'
  -- We only need to trim the end because the leading spaces and tab
  -- were already skipped.
  let !value = Bytes.dropWhileEnd (\c -> c == 0x20 || c == 0x09) value0
  pure Header {name = unsafeBytesToText name, value = unsafeBytesToText value}

unsafeBytesToText :: Bytes -> Text
{-# INLINE unsafeBytesToText #-}
unsafeBytesToText (Bytes (ByteArray arr) off len) =
  Text.Text (Data.Text.Array.ByteArray arr) off len

-- | Encode a header. Includes the trailing CRLF sequence.
builder :: Header -> Builder
builder Header {name, value} =
  Builder.copy (Utf8.fromText name)
    <> Builder.ascii2 ':' ' '
    <> Builder.copy (Utf8.fromText value)
    <> Builder.ascii2 '\r' '\n'

builderSmallArray :: SmallArray Header -> Builder
builderSmallArray = foldMap builder
