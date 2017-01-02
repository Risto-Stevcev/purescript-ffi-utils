module FFI.Util.Function
  ( apply
  , bind

  , call0
  , call1
  , call2
  , call3
  , call4
  , call5
  , call6
  , call7

  , callAff0r1
  , callAff0r1'
  , callAff1r1
  , callAff1r1'
  , callAff2r1
  , callAff2r1'
  , callAff3r1
  , callAff3r1'
  , callAff4r1
  , callAff4r1'

  , callAff0r2
  , callAff0r2'
  , callAff1r2
  , callAff1r2'
  , callAff2r2
  , callAff2r2'
  , callAff3r2
  , callAff3r2'
  , callAff4r2
  , callAff4r2'

  , callAff0r3
  , callAff0r3'
  , callAff1r3
  , callAff1r3'
  , callAff2r3
  , callAff2r3'
  , callAff3r3
  , callAff3r3'
  , callAff4r3
  , callAff4r3'

  , callAff0r4
  , callAff0r4'
  , callAff1r4
  , callAff1r4'
  , callAff2r4
  , callAff2r4'
  , callAff3r4
  , callAff3r4'
  , callAff4r4
  , callAff4r4'

  , listen0
  , listen1
  , listen2
  , listen3
  , listen4

  , listenTo0
  , listenTo1
  , listenTo2
  , listenTo3
  , listenTo4

  , listenToEff0
  , listenToEff1
  , listenToEff2
  , listenToEff3
  , listenToEff4
  ) where

import Prelude (($), Unit, pure)
import FFI.Util (isNullOrUndefined)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Aff (Aff, makeAff)
import Data.Maybe (Maybe, maybe)
import Data.Function.Eff (mkEffFn1, mkEffFn2, mkEffFn3, mkEffFn4)
import Data.Function.Uncurried ( Fn2, Fn3, Fn4, Fn5, Fn6, Fn7, Fn8, Fn9
                               , runFn2, runFn3, runFn4, runFn5, runFn6, runFn7, runFn8, runFn9
                               , mkFn0, mkFn1, mkFn2, mkFn3, mkFn4, mkFn5 )


type Method = String
type Event = String

foreign import mkError :: forall a. a -> Maybe Error

foreign import apply :: forall f a b. f -> Array a -> b

foreign import bind :: forall f g a. f -> a -> g
infixl 6 bind as |.|

foreign import _call0 :: forall o b. Fn2 o Method b
foreign import _call1 :: forall o a1 b. Fn3 o Method a1 b
foreign import _call2 :: forall o a1 a2 b. Fn4 o Method a1 a2 b
foreign import _call3 :: forall o a1 a2 a3 b. Fn5 o Method a1 a2 a3 b
foreign import _call4 :: forall o a1 a2 a3 a4 b. Fn6 o Method a1 a2 a3 a4 b
foreign import _call5 :: forall o a1 a2 a3 a4 a5 b. Fn7 o Method a1 a2 a3 a4 a5 b
foreign import _call6 :: forall o a1 a2 a3 a4 a5 a6 b. Fn8 o Method a1 a2 a3 a4 a5 a6 b
foreign import _call7 :: forall o a1 a2 a3 a4 a5 a6 a7 b. Fn9 o Method a1 a2 a3 a4 a5 a6 a7 b

call0 :: forall o b. o -> Method -> b
call0 = runFn2 _call0

call1 :: forall o a1 b. o -> Method -> a1 -> b
call1 = runFn3 _call1

call2 :: forall o a1 a2 b. o -> Method -> a1 -> a2 -> b
call2 = runFn4 _call2

call3 :: forall o a1 a2 a3 b. o -> Method -> a1 -> a2 -> a3 -> b
call3 = runFn5 _call3

call4 :: forall o a1 a2 a3 a4 b. o -> Method -> a1 -> a2 -> a3 -> a4 -> b
call4 = runFn6 _call4

call5 :: forall o a1 a2 a3 a4 a5 b. o -> Method -> a1 -> a2 -> a3 -> a4 -> a5 -> b
call5 = runFn7 _call5

call6 :: forall o a1 a2 a3 a4 a5 a6 b. o -> Method -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
call6 = runFn8 _call6

call7 :: forall o a1 a2 a3 a4 a5 a6 a7 b. o -> Method -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
call7 = runFn9 _call7


callAff0r1 :: forall o b c. o -> Method -> Aff b c
callAff0r1 o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn2 \err res ->
    unsafePerformEff $ if (isNullOrUndefined err) then success res else error err

callAff1r1 :: forall o a1 b c. o -> Method -> a1 -> Aff b c
callAff1r1 o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn2 \err res ->
    unsafePerformEff $ if (isNullOrUndefined err) then success res else error err

callAff2r1 :: forall o a1 a2 b c. o -> Method -> a1 -> a2 -> Aff b c
callAff2r1 o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn2 \err res ->
    unsafePerformEff $ if (isNullOrUndefined err) then success res else error err

callAff3r1 :: forall o a1 a2 a3 b c. o -> Method -> a1 -> a2 -> a3 -> Aff b c
callAff3r1 o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn2 \err res ->
    unsafePerformEff $ if (isNullOrUndefined err) then success res else error err

callAff4r1 :: forall o a1 a2 a3 a4 b c. o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b c
callAff4r1 o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \err res ->
    unsafePerformEff $ if (isNullOrUndefined err) then success res else error err


callAff0r2 :: forall o b r1 r2. o -> Method -> Aff b {res1 :: r1, res2 :: r2}
callAff0r2 o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn3 \err res1 res2 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2} else error err

callAff1r2 :: forall o a1 b r1 r2. o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2}
callAff1r2 o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn3 \err res1 res2 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2} else error err

callAff2r2 :: forall o a1 a2 b r1 r2. o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2}
callAff2r2 o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn3 \err res1 res2 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2} else error err

callAff3r2 :: forall o a1 a2 a3 b r1 r2. o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2}
callAff3r2 o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn3 \err res1 res2 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2} else error err

callAff4r2 :: forall o a1 a2 a3 a4 b r1 r2. o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2}
callAff4r2 o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \err res1 res2 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2} else error err


callAff0r3 :: forall o b r1 r2 r3
            . o -> Method -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff0r3 o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn4 \err res1 res2 res3 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3} else error err

callAff1r3 :: forall o a1 b r1 r2 r3
            . o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff1r3 o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn4 \err res1 res2 res3 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3} else error err

callAff2r3 :: forall o a1 a2 b r1 r2 r3
            . o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff2r3 o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn4 \err res1 res2 res3 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3} else error err

callAff3r3 :: forall o a1 a2 a3 b r1 r2 r3
            . o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff3r3 o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn4 \err res1 res2 res3 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3} else error err

callAff4r3 :: forall o a1 a2 a3 a4 b r1 r2 r3
            . o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff4r3 o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \err res1 res2 res3 ->
    unsafePerformEff $ if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3} else error err


callAff0r4 :: forall o b r1 r2 r3 r4
            . o -> Method -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff0r4 o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn5 \err res1 res2 res3 res4 -> unsafePerformEff $
    if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3, res4: res4} else error err


callAff1r4 :: forall o a1 b r1 r2 r3 r4
            . o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff1r4 o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn5 \err res1 res2 res3 res4 -> unsafePerformEff $
    if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3, res4: res4} else error err


callAff2r4 :: forall o a1 a2 b r1 r2 r3 r4
            . o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff2r4 o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn5 \err res1 res2 res3 res4 -> unsafePerformEff $
    if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3, res4: res4} else error err


callAff3r4 :: forall o a1 a2 a3 b r1 r2 r3 r4
            . o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff3r4 o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn5 \err res1 res2 res3 res4 -> unsafePerformEff $
    if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3, res4: res4} else error err


callAff4r4 :: forall o a1 a2 a3 a4 b r1 r2 r3 r4
            . o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff4r4 o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn5 \err res1 res2 res3 res4 -> unsafePerformEff $
    if (isNullOrUndefined err) then success {res1: res1, res2: res2, res3: res3, res4: res4} else error err




callAff0r1' :: forall o b c. o -> Method -> Aff b c
callAff0r1' o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn1 \res ->
    unsafePerformEff $ maybe (success res) error (mkError res)

callAff1r1' :: forall o a1 b c. o -> Method -> a1 -> Aff b c
callAff1r1' o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn1 \res ->
    unsafePerformEff $ maybe (success res) error (mkError res)

callAff2r1' :: forall o a1 a2 b c. o -> Method -> a1 -> a2 -> Aff b c
callAff2r1' o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn1 \res ->
    unsafePerformEff $ maybe (success res) error (mkError res)

callAff3r1' :: forall o a1 a2 a3 b c. o -> Method -> a1 -> a2 -> a3 -> Aff b c
callAff3r1' o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn1 \res ->
    unsafePerformEff $ maybe (success res) error (mkError res)

callAff4r1' :: forall o a1 a2 a3 a4 b c. o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b c
callAff4r1' o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn1 \res ->
    unsafePerformEff $ maybe (success res) error (mkError res)


callAff0r2' :: forall o b r1 r2. o -> Method -> Aff b {res1 :: r1, res2 :: r2}
callAff0r2' o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn2 \res1 res2 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2}) error (mkError res1)

callAff1r2' :: forall o a1 b r1 r2. o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2}
callAff1r2' o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn2 \res1 res2 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2}) error (mkError res1)

callAff2r2' :: forall o a1 a2 b r1 r2. o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2}
callAff2r2' o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn2 \res1 res2 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2}) error (mkError res1)

callAff3r2' :: forall o a1 a2 a3 b r1 r2. o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2}
callAff3r2' o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn2 \res1 res2 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2}) error (mkError res1)

callAff4r2' :: forall o a1 a2 a3 a4 b r1 r2. o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2}
callAff4r2' o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \res1 res2 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2}) error (mkError res1)


callAff0r3' :: forall o b r1 r2 r3
             . o -> Method -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff0r3' o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn3 \res1 res2 res3 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3}) error (mkError res1)

callAff1r3' :: forall o a1 b r1 r2 r3
             . o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff1r3' o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn3 \res1 res2 res3 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3}) error (mkError res1)

callAff2r3' :: forall o a1 a2 b r1 r2 r3
             . o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff2r3' o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn3 \res1 res2 res3 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3}) error (mkError res1)

callAff3r3' :: forall o a1 a2 a3 b r1 r2 r3
             . o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff3r3' o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn3 \res1 res2 res3 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3}) error (mkError res1)

callAff4r3' :: forall o a1 a2 a3 a4 b r1 r2 r3
             . o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3}
callAff4r3' o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \res1 res2 res3 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3}) error (mkError res1)


callAff0r4' :: forall o b r1 r2 r3 r4
             . o -> Method -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff0r4' o m = makeAff $ \error success ->
  pure $ call1 o m $ mkFn4 \res1 res2 res3 res4 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3, res4: res4}) error (mkError res1)


callAff1r4' :: forall o a1 b r1 r2 r3 r4
             . o -> Method -> a1 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff1r4' o m a1 = makeAff $ \error success ->
  pure $ call2 o m a1 $ mkFn4 \res1 res2 res3 res4 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3, res4: res4}) error (mkError res1)


callAff2r4' :: forall o a1 a2 b r1 r2 r3 r4
             . o -> Method -> a1 -> a2 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff2r4' o m a1 a2 = makeAff $ \error success ->
  pure $ call3 o m a1 a2 $ mkFn4 \res1 res2 res3 res4 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3, res4: res4}) error (mkError res1)


callAff3r4' :: forall o a1 a2 a3 b r1 r2 r3 r4
             . o -> Method -> a1 -> a2 -> a3 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff3r4' o m a1 a2 a3 = makeAff $ \error success ->
  pure $ call4 o m a1 a2 a3 $ mkFn4 \res1 res2 res3 res4 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3, res4: res4}) error (mkError res1)


callAff4r4' :: forall o a1 a2 a3 a4 b r1 r2 r3 r4
             . o -> Method -> a1 -> a2 -> a3 -> a4 -> Aff b {res1 :: r1, res2 :: r2, res3 :: r3, res4 :: r4}
callAff4r4' o m a1 a2 a3 a4 = makeAff $ \error success ->
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \res1 res2 res3 res4 ->
    unsafePerformEff $ maybe (success {res1: res1, res2: res2, res3: res3, res4: res4}) error (mkError res1)




listen0 :: forall o e. o -> Method -> (Unit -> Unit) -> Eff e Unit
listen0 o m g = pure $ call1 o m (mkFn0 g)

listen1 :: forall o e a1. o -> Method -> (a1 -> Unit) -> Eff e Unit
listen1 o m g = pure $ call1 o m (mkFn1 g)

listen2 :: forall o e a1 a2. o -> Method -> (a1 -> a2 -> Unit) -> Eff e Unit
listen2 o m g = pure $ call1 o m (mkFn2 g)

listen3 :: forall o e a1 a2 a3. o -> Method -> (a1 -> a2 -> a3 -> Unit) -> Eff e Unit
listen3 o m g = pure $ call1 o m (mkFn3 g)

listen4 :: forall o e a1 a2 a3 a4. o -> Method -> (a1 -> a2 -> a3 -> a4 -> Unit) -> Eff e Unit
listen4 o m g = pure $ call1 o m (mkFn4 g)


listenTo0 :: forall o e. o -> Method -> Event -> (Unit -> Unit) -> Eff e Unit
listenTo0 o m e g = pure $ call2 o m e (mkFn0 g)

listenTo1 :: forall o e a1. o -> Method -> Event -> (a1 -> Unit) -> Eff e Unit
listenTo1 o m e g = pure $ call2 o m e (mkFn1 g)

listenTo2 :: forall o e a1 a2. o -> Method -> Event -> (a1 -> a2 -> Unit) -> Eff e Unit
listenTo2 o m e g = pure $ call2 o m e (mkFn2 g)

listenTo3 :: forall o e a1 a2 a3. o -> Method -> Event -> (a1 -> a2 -> a3 -> Unit) -> Eff e Unit
listenTo3 o m e g = pure $ call2 o m e (mkFn3 g)

listenTo4 :: forall o e a1 a2 a3 a4. o -> Method -> Event -> (a1 -> a2 -> a3 -> a4 -> Unit) -> Eff e Unit
listenTo4 o m e g = pure $ call2 o m e (mkFn4 g)


listenToEff0 :: forall o e. o -> Method -> Event -> (Unit -> Eff e Unit) -> Eff e Unit
listenToEff0 o m e g = pure $ call2 o m e (mkEffFn1 g)

listenToEff1 :: forall o e a1. o -> Method -> Event -> (a1 -> Eff e Unit) -> Eff e Unit
listenToEff1 o m e g = pure $ call2 o m e (mkEffFn1 g)

listenToEff2 :: forall o e a1 a2. o -> Method -> Event -> (a1 -> a2 -> Eff e Unit) -> Eff e Unit
listenToEff2 o m e g = pure $ call2 o m e (mkEffFn2 g)

listenToEff3 :: forall o e a1 a2 a3. o -> Method -> Event -> (a1 -> a2 -> a3 -> Eff e Unit) -> Eff e Unit
listenToEff3 o m e g = pure $ call2 o m e (mkEffFn3 g)

listenToEff4 :: forall o e a1 a2 a3 a4. o -> Method -> Event -> (a1 -> a2 -> a3 -> a4 -> Eff e Unit) -> Eff e Unit
listenToEff4 o m e g = pure $ call2 o m e (mkEffFn4 g)
