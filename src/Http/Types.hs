{-# language DuplicateRecordFields #-}

module Http.Types
  ( -- * Request
    Request(..)
  , RequestLine(..)
    -- * Response
  , Response(..)
  , StatusLine(..)
    -- * Header
  , Header(..)
    -- * Bodied
  , Bodied(..)
  ) where

import Http.Message.Request (Request(..),RequestLine(..))
import Http.Message.Response (Response(..),StatusLine(..))
import Http.Header (Header(..))
import Http.Bodied (Bodied(..))
