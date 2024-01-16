{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Http.Response
  ( Response (..)
  , StatusLine (..)
  , decode
  ) where

import Control.Monad (when)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (ByteArray), SmallArray)
import Data.Text (Text)
import Data.Word (Word16, Word8)
import Http.Header (Header)
import Http.Headers (Headers)

import Data.Bytes.Parser qualified as Parser
import Data.Bytes.Parser.Latin qualified as Latin
import Data.Text.Array qualified
import Data.Text.Internal qualified as Text
import Http.Header qualified as Header
import Http.Headers qualified as Headers

-- | The response status line and the response headers.
data Response = Response
  { statusLine :: !StatusLine
  , headers :: !Headers
  }
  deriving (Show)

data StatusLine = StatusLine
  { statusCode :: !Word16
  , statusReason :: {-# UNPACK #-} !Text
  }
  deriving (Show)

{- | Decode the response status line and the response headers. Fails if
any extraneous input is present after the double CRLF sequence that
ends the headers.
-}
decode :: Int -> Bytes -> Maybe Response
decode !n !b =
  Parser.parseBytesMaybe (parserResponse n <* Parser.endOfInput ()) b

parserResponse ::
  -- | Maximum number of headers
  Int ->
  Parser () s Response
parserResponse !n = do
  statusLine <- parserStatusLine
  headers0 <- Header.parserSmallArray n
  let !headers = Headers.fromArray headers0
  pure Response {statusLine, headers}

-- Consumes the trailing CRLF
parserStatusLine :: Parser () s StatusLine
parserStatusLine = do
  Latin.char5 () 'H' 'T' 'T' 'P' '/'
  versionMajor <- Latin.decWord8 ()
  when (versionMajor /= 1) (Parser.fail ())
  Latin.char () '.'
  versionMinor <- Latin.decWord8 ()
  when (versionMinor /= 1) (Parser.fail ())
  Latin.char () ' '
  statusCode <- Latin.decWord16 ()
  when (statusCode >= 1000) (Parser.fail ())
  Latin.char () ' '
  -- RFC 7230: reason-phrase = *( HTAB / SP / VCHAR / obs-text )
  statusReason <- Parser.takeWhile $ \c ->
    (c >= 0x20 && c <= 0x7e)
      || (c == 0x09)
  Latin.char2 () '\r' '\n'
  pure StatusLine {statusCode, statusReason = unsafeBytesToText statusReason}

unsafeBytesToText :: Bytes -> Text
{-# INLINE unsafeBytesToText #-}
unsafeBytesToText (Bytes (ByteArray arr) off len) =
  Text.Text (Data.Text.Array.ByteArray arr) off len
