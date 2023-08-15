{-# language DuplicateRecordFields #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}

-- TODO: Right now, this uses a crummy implementation. Instead, we
-- should hash all the keys and search the hashes first to speed
-- things up.
module Http.Headers
  ( -- * Types
    Headers
  , LookupException(..)
    -- * Construct
  , fromArray
  , fromList
    -- * Expose
  , toArray
    -- * Lookup
  , lookup
  , lookupFirst
  , lookupAll
    -- * Specialized Lookup
  , lookupContentType
  , lookupContentLength
  , lookupTransferEncoding
  ) where

import Prelude hiding (lookup)

import Data.Text (Text)
import Data.Primitive (SmallArray)
import Http.Header (Header(Header))
import Data.Foldable (foldl')

import qualified Data.List as List
import qualified Data.Primitive.Contiguous as C
import qualified Data.Text as T
import qualified Http.Header
import qualified GHC.Exts as Exts

-- | Collection of HTTP headers. Supports case-insensitive lookup.
-- This is intended to be used for small collections of headers.
-- Expect degraded performance if this is used for collections of
-- more than 128 headers.
--
-- This preserves the original order of the headers and the original
-- case of the header names.
newtype Headers = Headers (SmallArray Header)
  deriving newtype (Show)

-- | Many headers cannot appear more than once. This is part of
-- the return type for 'lookup', and it helps us track whether the
-- lookup failure was the result of something that might be expected
-- (the header was @Missing@) or something that is definitely a mistake
-- (the header was duplicated).
data LookupException
  = Duplicate
  | Missing

-- | Convert array of headers to a 'Headers' collection that supports
-- efficient lookup.
fromArray :: SmallArray Header -> Headers
fromArray = Headers

-- | Convert list of headers to a 'Headers' collection that supports
-- efficient lookup.
fromList :: [Header] -> Headers
fromList = Headers . Exts.fromList

-- | Recover the original headers from from the 'Headers' collection.
-- This is @O(1)@ and is most commonly used to fold over the headers.
toArray :: Headers -> SmallArray Header
toArray (Headers xs) = xs

-- | Case insensitive lookup of an HTTP header. If the header is present,
-- returns both the original header name (may differs in case from the
-- header name searched for) and the header value. Only returns the first
-- occurrence of the header.
lookupFirst ::
     Text -- header name
  -> Headers
  -> Maybe Header
lookupFirst needle (Headers hdrs) =
  List.find (\Header{name} -> caseInsensitiveEq needle name) hdrs

-- | Lookup a header that should not appear more than one time and verify
-- that it did not occur more than once. If it appears more than once
-- (or less than once), returns a 'LookupException'.
lookup ::
     Text -- header name
  -> Headers
  -> Either LookupException Header
lookup needle hdrs@(Headers xs) = case lookupFirst needle hdrs of
  Nothing -> Left Missing
  Just hdr ->
    let count = foldl'
          (\acc Header{name} -> if caseInsensitiveEq needle name
            then acc + 1
            else acc
          ) (0 :: Int) xs
     in if count > 1 then Left Duplicate else Right hdr

-- | Lookup a header that may appear more than once. Some headers
-- (e.g. @Set-Cookie@, @X-Forwarded-For@) are allowed to appear multiple
-- times. This returns all the headers that matched along with their
-- original names.
lookupAll ::
     Text -- header name
  -> Headers
  -> SmallArray Header
lookupAll needle (Headers hdrs) =
  C.filter (\Header{name} -> caseInsensitiveEq needle name) hdrs
  
-- TODO: Make this not allocate
caseInsensitiveEq :: Text -> Text -> Bool
caseInsensitiveEq a b = T.toLower a == T.toLower b

lookupTransferEncoding :: Headers -> Either LookupException Header
lookupTransferEncoding = lookup "transfer-encoding"

lookupContentType :: Headers -> Either LookupException Header
lookupContentType = lookup "content-type"

lookupContentLength :: Headers -> Either LookupException Header
lookupContentLength = lookup "content-length"
