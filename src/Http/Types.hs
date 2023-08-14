{-# language DuplicateRecordFields #-}

module Http.Types
  ( -- * Request
    Request(..)
  , RequestLine(..)
    -- * Response
  , Response(..)
  , StatusLine(..)
    -- * Header
  , Headers
  , Header(..)
  , LookupException(..)
    -- * Bodied
  , Bodied(..)
  ) where

import Http.Request (Request(..),RequestLine(..))
import Http.Response (Response(..),StatusLine(..))
import Http.Header (Header(..))
import Http.Headers (Headers,LookupException(..))
import Http.Bodied (Bodied(..))
