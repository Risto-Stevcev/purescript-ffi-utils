module FFI.Util
  ( instanceof
  , stringify
  , require
  , parseOptions
  , isNullOrUndefined
  , property
  , propertyPath
  , new
  ) where


foreign import instanceof :: forall a b. a -> b -> Boolean

foreign import stringify :: forall r. Boolean -> { | r } -> String

foreign import require :: forall a. String -> a

foreign import parseOptions :: forall r1 r2. { | r1 } -> { | r2 }

foreign import isNullOrUndefined :: forall a. a -> Boolean

foreign import property :: forall a b. a -> String -> b

foreign import propertyPath :: forall a b. a -> Array String -> b

foreign import new :: forall a b. a -> b
