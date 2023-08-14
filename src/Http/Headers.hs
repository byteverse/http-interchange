{-# language DuplicateRecordFields #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

-- TODO: Right now, this uses a crummy implementation. Instead, we
-- should hash all the keys and search the hashes first to speed
-- things up.
module Http.Headers
  ( -- * Types
    Headers
    -- * Construct
  , fromArray
    -- * Expose
  , toArray
    -- * Lookup
  , lookup
  , lookupUnique
  , lookupAll
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

-- | Collection of HTTP headers. Supports case-insensitive lookup.
newtype Headers = Headers (SmallArray Header)
  deriving newtype (Show)

fromArray :: SmallArray Header -> Headers
fromArray = Headers

toArray :: Headers -> SmallArray Header
toArray (Headers xs) = xs

-- | Case insensitive lookup of an HTTP header. If the header is present,
-- returns both the original header name (may differs in case from the
-- header name searched for) and the header value. Only returns the first
-- occurrence of the header.
lookup ::
     Text -- header name
  -> Headers
  -> Maybe Header
lookup needle (Headers hdrs) =
  List.find (\Header{name} -> caseInsensitiveEq needle name) hdrs

-- | Lookup a header that should not appear more than one time and verify
-- that it did not occur more than once. If it appears more than once
-- (or less than once), then returns the number of times that it appeared,
-- wrapped in @Left@.
lookupUnique ::
     Text -- header name
  -> Headers
  -> Either Int Header
lookupUnique needle hdrs@(Headers xs) = case lookup needle hdrs of
  Nothing -> Left 0
  Just hdr ->
    let count = foldl'
          (\acc Header{name} -> if caseInsensitiveEq needle name
            then acc + 1
            else acc
          ) 0 xs
     in if count > 1 then Left count else Right hdr

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
