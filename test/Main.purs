module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Fiber)
import Test.FFI.Utils (TestEffects, main) as Test

main ∷ ∀ e. Eff (Test.TestEffects e) (Fiber (Test.TestEffects e) Unit)
main = Test.main
