module FFI.Util
  ( typeof
  , instanceof
  , stringify
  , require
  , parseOptions
  , isNullOrUndefined
  , property
  , propertyPath
  , property'
  , propertyPath'
  , setProperty
  , setPropertyPath
  , new
  , new0
  , new1
  , new2
  , new3
  , new4
  , new5
  , new6
  , new7
  , newEffect0
  , newEffect1
  , newEffect2
  , newEffect3
  , newEffect4
  , newEffect5
  , newEffect6
  , newEffect7
  , window
  , global
  , Window
  , Global
  ) where

import Prelude (pure, unit, Unit)
import Data.Maybe (Maybe(Nothing))
import Data.Function.Uncurried ( Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8
                               , runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8 )
import Effect.Uncurried ( EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, EffectFn8
                        , runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6, runEffectFn7, runEffectFn8 )
import Effect (Effect)

foreign import typeof ∷ ∀ a. a → String

foreign import instanceof ∷ ∀ a b. a → b → Boolean

foreign import stringify ∷ ∀ r. Boolean → { | r } → String

foreign import require ∷ ∀ a. String → a

foreign import _parseOptions ∷ ∀ a r1 r2. Fn3 (Maybe a) (Maybe a) { | r1 } { | r2 }

parseOptions ∷ ∀ r1 r2. { | r1 } → { | r2 }
parseOptions = runFn3 _parseOptions (pure unit) Nothing

foreign import isNullOrUndefined ∷ ∀ a. a → Boolean

foreign import property ∷ ∀ a b. a → String → b

foreign import propertyPath ∷ ∀ a b. a → Array String → b

-- Like property, except the object is window or global
foreign import property' ∷ ∀ a. String → a

-- Like propertyPath, except the object is window or global
foreign import propertyPath' ∷ ∀ a. Array String → a

foreign import setProperty ∷ ∀ a b. a → String → b → Unit

foreign import setPropertyPath ∷ ∀ a b. a → Array String → b → Unit

foreign import window ∷ Unit → Window

foreign import global ∷ Unit → Global

foreign import data Window ∷ Type

foreign import data Global ∷ Type

foreign import new ∷ ∀ a b. a → b

foreign import _new1 ∷ ∀ o a1 b. Fn2 o a1 b
foreign import _new2 ∷ ∀ o a1 a2 b. Fn3 o a1 a2 b
foreign import _new3 ∷ ∀ o a1 a2 a3 b. Fn4 o a1 a2 a3 b
foreign import _new4 ∷ ∀ o a1 a2 a3 a4 b. Fn5 o a1 a2 a3 a4 b
foreign import _new5 ∷ ∀ o a1 a2 a3 a4 a5 b. Fn6 o a1 a2 a3 a4 a5 b
foreign import _new6 ∷ ∀ o a1 a2 a3 a4 a5 a6 b. Fn7 o a1 a2 a3 a4 a5 a6 b
foreign import _new7 ∷ ∀ o a1 a2 a3 a4 a5 a6 a7 b. Fn8 o a1 a2 a3 a4 a5 a6 a7 b

new0 ∷ ∀ o b. o → b
new0 = new

new1 ∷ ∀ o a1 b. o → a1 → b
new1 = runFn2 _new1

new2 ∷ ∀ o a1 a2 b. o → a1 → a2 → b
new2 = runFn3 _new2

new3 ∷ ∀ o a1 a2 a3 b. o → a1 → a2 → a3 → b
new3 = runFn4 _new3

new4 ∷ ∀ o a1 a2 a3 a4 b. o → a1 → a2 → a3 → a4 → b
new4 = runFn5 _new4

new5 ∷ ∀ o a1 a2 a3 a4 a5 b. o → a1 → a2 → a3 → a4 → a5 → b
new5 = runFn6 _new5

new6 ∷ ∀ o a1 a2 a3 a4 a5 a6 b. o → a1 → a2 → a3 → a4 → a5 → a6 → b
new6 = runFn7 _new6

new7 ∷ ∀ o a1 a2 a3 a4 a5 a6 a7 b. o → a1 → a2 → a3 → a4 → a5 → a6 → a7 → b
new7 = runFn8 _new7

foreign import _newEffect0 ∷ ∀ o b. EffectFn1 o b
foreign import _newEffect1 ∷ ∀ o a1 b. EffectFn2 o a1 b
foreign import _newEffect2 ∷ ∀ o a1 a2 b. EffectFn3 o a1 a2 b
foreign import _newEffect3 ∷ ∀ o a1 a2 a3 b. EffectFn4 o a1 a2 a3 b
foreign import _newEffect4 ∷ ∀ o a1 a2 a3 a4 b. EffectFn5 o a1 a2 a3 a4 b
foreign import _newEffect5 ∷ ∀ o a1 a2 a3 a4 a5 b. EffectFn6 o a1 a2 a3 a4 a5 b
foreign import _newEffect6 ∷ ∀ o a1 a2 a3 a4 a5 a6 b. EffectFn7 o a1 a2 a3 a4 a5 a6 b
foreign import _newEffect7 ∷ ∀ o a1 a2 a3 a4 a5 a6 a7 b. EffectFn8 o a1 a2 a3 a4 a5 a6 a7 b

newEffect0 ∷ ∀ o b. o → Effect b
newEffect0 = runEffectFn1 _newEffect0

newEffect1 ∷ ∀ o a1 b. o → a1 → Effect b
newEffect1 = runEffectFn2 _newEffect1

newEffect2 ∷ ∀ o a1 a2 b. o → a1 → a2 → Effect b
newEffect2 = runEffectFn3 _newEffect2

newEffect3 ∷ ∀ o a1 a2 a3 b. o → a1 → a2 → a3 → Effect b
newEffect3 = runEffectFn4 _newEffect3

newEffect4 ∷ ∀ o a1 a2 a3 a4 b. o → a1 → a2 → a3 → a4 → Effect b
newEffect4 = runEffectFn5 _newEffect4

newEffect5 ∷ ∀ o a1 a2 a3 a4 a5 b. o → a1 → a2 → a3 → a4 → a5 → Effect b
newEffect5 = runEffectFn6 _newEffect5

newEffect6 ∷ ∀ o a1 a2 a3 a4 a5 a6 b. o → a1 → a2 → a3 → a4 → a5 → a6 → Effect b
newEffect6 = runEffectFn7 _newEffect6

newEffect7 ∷ ∀ o a1 a2 a3 a4 a5 a6 a7 b. o → a1 → a2 → a3 → a4 → a5 → a6 → a7 → Effect b
newEffect7 = runEffectFn8 _newEffect7
