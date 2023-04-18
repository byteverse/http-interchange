{-# language LambdaCase #-}

module Http.Message.Response
  ( Response(..)
  , StatusLine(..)
  , decode
  ) where

import Control.Monad (when)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (SmallArray,ByteArray(ByteArray))
import Data.Word (Word8,Word16)
import Data.Text (Text)
import Data.Bytes.Builder (Builder)
import Http.Header (Header)

import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes as Bytes
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Http.Header as Header

-- | The response status line and the response headers.
data Response = Response
  { statusLine :: !StatusLine
  , headers :: !(SmallArray Header)
  } deriving (Show)

data StatusLine = StatusLine
  { versionMajor :: !Word8
  , versionMinor :: !Word8
  , statusCode :: !Word16
  , statusReason :: {-# UNPACK #-} !Text
  } deriving (Show)

-- | Decode the response status line and the response headers. Fails if
-- any extraneous input is present after the double CRLF sequence that
-- ends the headers.
decode :: Int -> Bytes -> Maybe Response
decode !n !b =
  Parser.parseBytesMaybe (parserResponse n <* Parser.endOfInput ()) b

parserResponse ::
     Int -- ^ Maximum number of headers
  -> Parser () s Response
parserResponse !n = do
  statusLine <- parserStatusLine
  headers <- Header.parserSmallArray n
  pure Response{statusLine,headers}

-- Consumes the trailing CRLF
parserStatusLine :: Parser () s StatusLine
parserStatusLine = do
  Latin.char5 () 'H' 'T' 'T' 'P' '/'
  versionMajor <- Latin.decWord8 ()
  Latin.char () '.'
  versionMinor <- Latin.decWord8 ()
  Latin.char () ' '
  statusCode <- Latin.decWord16 ()
  when (statusCode >= 1000) (Parser.fail ())
  Latin.char () ' '
  -- RFC 7230: reason-phrase = *( HTAB / SP / VCHAR / obs-text )
  statusReason <- Parser.takeWhile $ \c ->
    (c >= 0x20 && c <= 0x7e)
    ||
    (c == 0x09)
  Latin.char2 () '\r' '\n'
  pure StatusLine{versionMajor,versionMinor,statusCode,statusReason=unsafeBytesToText statusReason}

unsafeBytesToText :: Bytes -> Text
{-# inline unsafeBytesToText #-}
unsafeBytesToText (Bytes (ByteArray arr) off len) =
  Text.Text (Data.Text.Array.ByteArray arr) off len
