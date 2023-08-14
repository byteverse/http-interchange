{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}

module Http.Request
  ( Request(..)
  , RequestLine(..)
    -- * Encode Request
  , builder
  , toChunks
  , toChunksOnto
    -- * Encode Request with Body
  , bodiedToChunks
  ) where

import Data.Bytes.Builder (Builder)
import Data.Bytes.Chunks (Chunks)
import Data.Primitive (SmallArray)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts (Ptr(Ptr))
import Http.Bodied (Bodied(..))
import Http.Header (Header)

import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as Chunks
import qualified Http.Header as Header

-- | The request line and the request headers.
data Request = Request
  { requestLine :: !RequestLine
  , headers :: !(SmallArray Header)
  } deriving (Show)

-- | An HTTP request line
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

toChunksOnto :: Request -> Chunks -> Chunks
toChunksOnto r ch = Builder.runOnto 256 (builder r) ch

builder :: Request -> Builder
builder Request{requestLine,headers} =
  builderRequestLine requestLine
  <>
  Header.builderSmallArray headers
  <>
  Builder.ascii2 '\r' '\n'

-- | This adds the Content-Length header. It must not already
-- be present.
bodiedToChunks :: Bodied Request -> Chunks
bodiedToChunks Bodied{metadata=Request{requestLine,headers},body} =
  Builder.runOnto 256
    ( builderRequestLine requestLine
      <>
      Header.builderSmallArray headers
      <>
      Builder.cstring (Ptr "Content-Length: "#)
      <>
      Builder.word64Dec (fromIntegral (Chunks.length body))
      <>
      Builder.ascii4 '\r' '\n' '\r' '\n'
    ) body
