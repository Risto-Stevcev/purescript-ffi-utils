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
  , newEff0
  , newEff1
  , newEff2
  , newEff3
  , newEff4
  , newEff5
  , newEff6
  , newEff7
  , window
  , global
  , Window
  , Global
  ) where

import Prelude (pure, unit, Unit)
import Data.Maybe (Maybe(Nothing))
import Data.Function.Uncurried ( Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8
                               , runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8 )
import Data.Function.Eff ( EffFn1, EffFn2, EffFn3, EffFn4, EffFn5, EffFn6, EffFn7, EffFn8
                         , runEffFn1, runEffFn2, runEffFn3, runEffFn4, runEffFn5, runEffFn6, runEffFn7, runEffFn8 )
import Control.Monad.Eff (Eff)

foreign import typeof :: forall a. a -> String

foreign import instanceof :: forall a b. a -> b -> Boolean

foreign import stringify :: forall r. Boolean -> { | r } -> String

foreign import require :: forall a. String -> a

foreign import _parseOptions :: forall a r1 r2. Fn3 (Maybe a) (Maybe a) { | r1 } { | r2 }

parseOptions :: forall r1 r2. { | r1 } -> { | r2 }
parseOptions = runFn3 _parseOptions (pure unit) Nothing

foreign import isNullOrUndefined :: forall a. a -> Boolean

foreign import property :: forall a b. a -> String -> b

foreign import propertyPath :: forall a b. a -> Array String -> b

-- Like property, except the object is window or global
foreign import property' :: forall a. String -> a

-- Like propertyPath, except the object is window or global
foreign import propertyPath' :: forall a. Array String -> a

foreign import setProperty :: forall a b. a -> String -> b -> Unit

foreign import setPropertyPath :: forall a b. a -> Array String -> b -> Unit

foreign import window :: Unit -> Window

foreign import global :: Unit -> Global

foreign import data Window :: *

foreign import data Global :: *

foreign import new :: forall a b. a -> b

foreign import _new1 :: forall o a1 b. Fn2 o a1 b
foreign import _new2 :: forall o a1 a2 b. Fn3 o a1 a2 b
foreign import _new3 :: forall o a1 a2 a3 b. Fn4 o a1 a2 a3 b
foreign import _new4 :: forall o a1 a2 a3 a4 b. Fn5 o a1 a2 a3 a4 b
foreign import _new5 :: forall o a1 a2 a3 a4 a5 b. Fn6 o a1 a2 a3 a4 a5 b
foreign import _new6 :: forall o a1 a2 a3 a4 a5 a6 b. Fn7 o a1 a2 a3 a4 a5 a6 b
foreign import _new7 :: forall o a1 a2 a3 a4 a5 a6 a7 b. Fn8 o a1 a2 a3 a4 a5 a6 a7 b

new0 :: forall o b. o -> b
new0 = new

new1 :: forall o a1 b. o -> a1 -> b
new1 = runFn2 _new1

new2 :: forall o a1 a2 b. o -> a1 -> a2 -> b
new2 = runFn3 _new2

new3 :: forall o a1 a2 a3 b. o -> a1 -> a2 -> a3 -> b
new3 = runFn4 _new3

new4 :: forall o a1 a2 a3 a4 b. o -> a1 -> a2 -> a3 -> a4 -> b
new4 = runFn5 _new4

new5 :: forall o a1 a2 a3 a4 a5 b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> b
new5 = runFn6 _new5

new6 :: forall o a1 a2 a3 a4 a5 a6 b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
new6 = runFn7 _new6

new7 :: forall o a1 a2 a3 a4 a5 a6 a7 b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
new7 = runFn8 _new7

foreign import _newEff0 :: forall eff o b. EffFn1 eff o b
foreign import _newEff1 :: forall eff o a1 b. EffFn2 eff o a1 b
foreign import _newEff2 :: forall eff o a1 a2 b. EffFn3 eff o a1 a2 b
foreign import _newEff3 :: forall eff o a1 a2 a3 b. EffFn4 eff o a1 a2 a3 b
foreign import _newEff4 :: forall eff o a1 a2 a3 a4 b. EffFn5 eff o a1 a2 a3 a4 b
foreign import _newEff5 :: forall eff o a1 a2 a3 a4 a5 b. EffFn6 eff o a1 a2 a3 a4 a5 b
foreign import _newEff6 :: forall eff o a1 a2 a3 a4 a5 a6 b. EffFn7 eff o a1 a2 a3 a4 a5 a6 b
foreign import _newEff7 :: forall eff o a1 a2 a3 a4 a5 a6 a7 b. EffFn8 eff o a1 a2 a3 a4 a5 a6 a7 b

newEff0 :: forall o eff b. o -> Eff eff b
newEff0 = runEffFn1 _newEff0

newEff1 :: forall o a1 eff b. o -> a1 -> Eff eff b
newEff1 = runEffFn2 _newEff1

newEff2 :: forall o a1 a2 eff b. o -> a1 -> a2 -> Eff eff b
newEff2 = runEffFn3 _newEff2

newEff3 :: forall o a1 a2 a3 eff b. o -> a1 -> a2 -> a3 -> Eff eff b
newEff3 = runEffFn4 _newEff3

newEff4 :: forall o a1 a2 a3 a4 eff b. o -> a1 -> a2 -> a3 -> a4 -> Eff eff b
newEff4 = runEffFn5 _newEff4

newEff5 :: forall o a1 a2 a3 a4 a5 eff b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> Eff eff b
newEff5 = runEffFn6 _newEff5

newEff6 :: forall o a1 a2 a3 a4 a5 a6 eff b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Eff eff b
newEff6 = runEffFn7 _newEff6

newEff7 :: forall o a1 a2 a3 a4 a5 a6 a7 eff b. o -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Eff eff b
newEff7 = runEffFn8 _newEff7
