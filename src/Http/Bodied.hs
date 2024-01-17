module Http.Bodied
  ( Bodied (..)
  ) where

import Data.Bytes.Chunks (Chunks)

-- | An HTTP request or response with a body.
data Bodied a = Bodied
  { metadata :: !a
  -- ^ The request or response.
  , body :: !Chunks
  -- ^ The body.
  }
  deriving (Eq, Show)
