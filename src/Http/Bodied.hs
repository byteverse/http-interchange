module Http.Bodied
  ( Bodied(..)
  ) where

import Data.Bytes.Chunks (Chunks)

data Bodied a = Bodied
  { metadata :: !a
  , body :: !Chunks
  }
