module FFI.Util.Class
  ( class Taggable
  , class Untaggable
  , tag
  , untag
  ) where

import Foreign (Foreign)


class Taggable a where
  tag ∷ Foreign → a

class Untaggable a where
  untag ∷ a → Foreign
