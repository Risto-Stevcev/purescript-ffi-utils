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
  , window
  , global
  , Window
  , Global
  ) where

import Prelude (Unit)

foreign import typeof :: forall a. a -> String

foreign import instanceof :: forall a b. a -> b -> Boolean

foreign import stringify :: forall r. Boolean -> { | r } -> String

foreign import require :: forall a. String -> a

foreign import parseOptions :: forall r1 r2. { | r1 } -> { | r2 }

foreign import isNullOrUndefined :: forall a. a -> Boolean

foreign import property :: forall a b. a -> String -> b

foreign import propertyPath :: forall a b. a -> Array String -> b

-- Like property, except the object is window or global
foreign import property' :: forall a. String -> a

-- Like propertyPath, except the object is window or global
foreign import propertyPath' :: forall a. Array String -> a

foreign import setProperty :: forall a b. a -> String -> b -> Unit

foreign import setPropertyPath :: forall a b. a -> Array String -> b -> Unit

foreign import new :: forall a b. a -> b

foreign import window :: Unit -> Window

foreign import global :: Unit -> Global

foreign import data Window :: *

foreign import data Global :: *
