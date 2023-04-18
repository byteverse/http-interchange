{-# language LambdaCase #-}

module Http.Message.Request
  ( Request(..)
  , RequestLine(..)
  , builder
  , toChunks
  ) where

import Control.Monad (when)
import Data.Bytes.Chunks (Chunks)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (SmallArray,ByteArray(ByteArray))
import Data.Word (Word8,Word16)
import Data.Text (Text)
import Data.Bytes.Builder (Builder)
import Http.Header (Header)

import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes as Bytes
import qualified Data.Text.Internal as Text
import qualified Data.Text.Array
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes.Parser.Latin as Latin
import qualified Http.Header as Header

-- | The response status line and the response headers.
data Request = Request
  { requestLine :: !RequestLine
  , headers :: !(SmallArray Header)
  } deriving (Show)

data RequestLine = RequestLine
  { method :: {-# UNPACK #-} !Text
  , path :: {-# UNPACK #-} !Text
  , versionMajor :: !Word8
  , versionMinor :: !Word8
  } deriving (Show)

builderRequestLine :: RequestLine -> Builder
builderRequestLine RequestLine{method,path,versionMajor,versionMinor} =
  Builder.copy (Utf8.fromText method)
  <>
  Builder.ascii ' '
  <>
  Builder.copy (Utf8.fromText path)
  <>
  Builder.ascii6 ' ' 'H' 'T' 'T' 'P' '/'
  <>
  Builder.word8Dec versionMajor
  <>
  Builder.ascii '.'
  <>
  Builder.word8Dec versionMinor
  <>
  Builder.ascii2 '\r' '\n'

toChunks :: Request -> Chunks
toChunks = Builder.run 256 . builder

builder :: Request -> Builder
builder Request{requestLine,headers} =
  builderRequestLine requestLine
  <>
  Header.builderSmallArray headers
  <>
  Builder.ascii2 '\r' '\n'
