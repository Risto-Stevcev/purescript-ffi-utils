module FFI.Util.Log where

import Prelude (Unit)
import Effect (Effect)

-- | Logs any kind of object
foreign import logAny ∷ ∀ a . a → Effect Unit

-- | Like logAny except restricted to records
foreign import logObject ∷ ∀ r. { | r } → Effect Unit

-- | Stringifies and pretty prints the object before logging it
foreign import logStringify
  ∷ ∀ r. { | r } → Effect Unit
