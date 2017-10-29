module FFI.Util.Log where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)

-- | Logs any kind of object
foreign import logAny ∷ ∀ a e. a → Eff (console ∷ CONSOLE | e) Unit

-- | Like logAny except restricted to records
foreign import logObject ∷ ∀ e r. { | r } → Eff (console ∷ CONSOLE | e) Unit

-- | Stringifies and pretty prints the object before logging it
foreign import logStringify
  ∷ ∀ e r. { | r } → Eff (console ∷ CONSOLE, err ∷ EXCEPTION | e) Unit
