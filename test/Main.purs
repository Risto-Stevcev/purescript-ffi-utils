module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Runner (RunnerEffects)
import Test.FFI.Utils (TestEffects, main) as Test

main ∷ Effect (RunnerEffects (Test.TestEffects ())) Unit
main = Test.main
