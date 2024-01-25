{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}

module Http.Request
  ( Request (..)
  , RequestLine (..)

    -- * Encode Request
  , builder
  , toChunks
  , toChunksOnto

    -- * Encode Request with Body
  , bodiedToChunks
  ) where

import Data.Bytes.Builder (Builder)
import Data.Bytes.Chunks (Chunks)
import Data.Text (Text)
import GHC.Exts (Ptr (Ptr))
import Http.Bodied (Bodied (..))
import Http.Headers (Headers)

import Data.Bytes.Builder qualified as Builder
import Data.Bytes.Chunks qualified as Chunks
import Data.Bytes.Text.Utf8 qualified as Utf8
import Http.Header qualified as Header
import Http.Headers qualified as Headers

-- | The request line and the request headers.
data Request = Request
  { requestLine :: !RequestLine
  , headers :: !Headers
  }
  deriving (Eq, Show)

-- | An HTTP request line
data RequestLine = RequestLine
  { method :: {-# UNPACK #-} !Text
  , path :: {-# UNPACK #-} !Text
  }
  deriving (Eq, Show)

builderRequestLine :: RequestLine -> Builder
builderRequestLine RequestLine {method, path} =
  Builder.copy (Utf8.fromText method)
    <> Builder.ascii ' '
    <> Builder.copy (Utf8.fromText path)
    <> Builder.ascii6 ' ' 'H' 'T' 'T' 'P' '/'
    <> Builder.ascii5 '1' '.' '1' '\r' '\n'

toChunks :: Request -> Chunks
toChunks = Builder.run 256 . builder

toChunksOnto :: Request -> Chunks -> Chunks
toChunksOnto r = Builder.runOnto 256 (builder r)

builder :: Request -> Builder
builder Request {requestLine, headers} =
  builderRequestLine requestLine
    <> Header.builderSmallArray headersArray
    <> Builder.ascii2 '\r' '\n'
 where
  headersArray = Headers.toArray headers

{- | This adds the Content-Length header. It must not already
be present.
-}
bodiedToChunks :: Bodied Request -> Chunks
bodiedToChunks Bodied {metadata = Request {requestLine, headers}, body} =
  Builder.runOnto
    256
    ( builderRequestLine requestLine
        <> Header.builderSmallArray headersArray
        <> Builder.cstring (Ptr "Content-Length: "#)
        <> Builder.word64Dec (fromIntegral (Chunks.length body))
        <> Builder.ascii4 '\r' '\n' '\r' '\n'
    )
    body
 where
  headersArray = Headers.toArray headers
