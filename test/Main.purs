module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects)
import Test.FFI.Utils (TestEffects, main) as Test

main ∷ Eff (RunnerEffects (Test.TestEffects ())) Unit
main = Test.main
