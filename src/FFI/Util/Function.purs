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

  , callEffect0
  , callEffect1
  , callEffect2
  , callEffect3
  , callEffect4
  , callEffect5
  , callEffect6

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

  , listenToEffect0
  , listenToEffect1
  , listenToEffect2
  , listenToEffect3
  , listenToEffect4
  ) where

import Control.Category ((<<<))
import Effect.Aff (Aff, makeAff)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn10, EffectFn2, EffectFn5, EffectFn6, EffectFn7, EffectFn8, EffectFn9, mkEffectFn1, mkEffectFn2, mkEffectFn3, mkEffectFn4, runEffectFn10, runEffectFn2, runEffectFn5, runEffectFn6, runEffectFn7, runEffectFn8, runEffectFn9)
import Effect.Unsafe (unsafePerformEffect)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn10, Fn2, Fn5, Fn6, Fn7, Fn8, Fn9, mkFn0, mkFn1, mkFn2, mkFn3, mkFn4, mkFn5, runFn10, runFn2, runFn5, runFn6, runFn7, runFn8, runFn9)
import Data.Maybe (Maybe(..), maybe)
import FFI.Util (isNullOrUndefined)
import Prelude (($), Unit, pure)


type Method = String
type Event = String

foreign import _mkError ∷ ∀ a m. (m → Maybe m) → (Maybe m) → a → Maybe Error
mkError ∷ ∀ a. a → Maybe Error
mkError = _mkError Just Nothing

foreign import apply ∷ ∀ f a b. f → Array a → b

foreign import bind ∷ ∀ f g a. f → a → g
infixl 6 bind as |.|

foreign import _call0 ∷ ∀ o b. Fn2 o Method b
foreign import _call1 ∷ ∀ m o a1 b. Fn5 (m → Maybe m) (Maybe m) o Method a1 b
foreign import _call2 ∷ ∀ m o a1 a2 b. Fn6 (m → Maybe m) (Maybe m) o Method a1 a2 b
foreign import _call3 ∷ ∀ m o a1 a2 a3 b. Fn7 (m → Maybe m) (Maybe m) o Method a1 a2 a3 b
foreign import _call4 ∷ ∀ m o a1 a2 a3 a4 b. Fn8 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 b
foreign import _call5 ∷ ∀ m o a1 a2 a3 a4 a5 b. Fn9 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 b
foreign import _call6
  ∷ ∀ m o a1 a2 a3 a4 a5 a6 b. Fn10 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 a6 b

call0 ∷ ∀ o b. o → Method → b
call0 = runFn2 _call0

call1 ∷ ∀ o a1 b. o → Method → a1 → b
call1 = runFn5 _call1 Just Nothing

call2 ∷ ∀ o a1 a2 b. o → Method → a1 → a2 → b
call2 = runFn6 _call2 Just Nothing

call3 ∷ ∀ o a1 a2 a3 b. o → Method → a1 → a2 → a3 → b
call3 = runFn7 _call3 Just Nothing

call4 ∷ ∀ o a1 a2 a3 a4 b. o → Method → a1 → a2 → a3 → a4 → b
call4 = runFn8 _call4 Just Nothing

call5 ∷ ∀ o a1 a2 a3 a4 a5 b. o → Method → a1 → a2 → a3 → a4 → a5 → b
call5 = runFn9 _call5 Just Nothing

call6 ∷ ∀ o a1 a2 a3 a4 a5 a6 b. o → Method → a1 → a2 → a3 → a4 → a5 → a6 → b
call6 = runFn10 _call6 Just Nothing


foreign import _callEffect0 ∷ ∀ o b. EffectFn2 o Method b
foreign import _callEffect1
  ∷ ∀ m o a1 b. EffectFn5 (m → Maybe m) (Maybe m) o Method a1 b
foreign import _callEffect2
  ∷ ∀ m o a1 a2 b. EffectFn6 (m → Maybe m) (Maybe m) o Method a1 a2 b
foreign import _callEffect3
  ∷ ∀ m o a1 a2 a3 b. EffectFn7 (m → Maybe m) (Maybe m) o Method a1 a2 a3 b
foreign import _callEffect4
  ∷ ∀ m o a1 a2 a3 a4 b. EffectFn8 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 b
foreign import _callEffect5
  ∷ ∀ m o a1 a2 a3 a4 a5 b. EffectFn9 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 b
foreign import _callEffect6
  ∷ ∀ m o a1 a2 a3 a4 a5 a6 b. EffectFn10 (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 a6 b

callEffect0 ∷ ∀ o b. o → Method → Effect b
callEffect0 = runEffectFn2 _callEffect0

callEffect1 ∷ ∀ o a1 b. o → Method → a1 → Effect b
callEffect1 = runEffectFn5 _callEffect1 Just Nothing

callEffect2 ∷ ∀ o a1 a2 b. o → Method → a1 → a2 → Effect b
callEffect2 = runEffectFn6 _callEffect2 Just Nothing

callEffect3 ∷ ∀ o a1 a2 a3 b. o → Method → a1 → a2 → a3 → Effect b
callEffect3 = runEffectFn7 _callEffect3 Just Nothing

callEffect4 ∷ ∀ o a1 a2 a3 a4 b. o → Method → a1 → a2 → a3 → a4 → Effect b
callEffect4 = runEffectFn8 _callEffect4 Just Nothing

callEffect5
  ∷ ∀ o a1 a2 a3 a4 a5 b. o → Method → a1 → a2 → a3 → a4 → a5 → Effect b
callEffect5 = runEffectFn9 _callEffect5 Just Nothing

callEffect6
  ∷ ∀ o a1 a2 a3 a4 a5 a6 b
  . o → Method → a1 → a2 → a3 → a4 → a5 → a6 → Effect b
callEffect6 = runEffectFn10 _callEffect6 Just Nothing



callAff0r1 ∷ ∀ o c. o → Method → Aff c
callAff0r1 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn2 \err res →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff1r1 ∷ ∀ o a1 c. o → Method → a1 → Aff c
callAff1r1 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn2 \err res →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff2r1 ∷ ∀ o a1 a2 c. o → Method → a1 → a2 → Aff c
callAff2r1 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn2 \err res →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff3r1 ∷ ∀ o a1 a2 a3 c. o → Method → a1 → a2 → a3 → Aff c
callAff3r1 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn2 \err res →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff4r1 ∷ ∀ o a1 a2 a3 a4 c. o → Method → a1 → a2 → a3 → a4 → Aff c
callAff4r1 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \err res →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)


callAff0r2 ∷ ∀ o r1 r2. o → Method → Aff {res1 ∷ r1, res2 ∷ r2}
callAff0r2 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn3 \err res1 res2 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff1r2 ∷ ∀ o a1 r1 r2. o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff1r2 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn3 \err res1 res2 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff2r2
  ∷ ∀ o a1 a2 r1 r2. o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff2r2 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn3 \err res1 res2 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff3r2
  ∷ ∀ o a1 a2 a3 r1 r2
  . o → Method → a1 → a2 → a3 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff3r2 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn3 \err res1 res2 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff4r2
  ∷ ∀ o a1 a2 a3 a4 r1 r2
  . o → Method → a1 → a2 → a3 → a4 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff4r2 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \err res1 res2 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)


callAff0r3 ∷ ∀ o r1 r2 r3
            . o → Method → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff0r3 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn4 \err res1 res2 res3 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff1r3 ∷ ∀ o a1 r1 r2 r3
            . o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff1r3 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff2r3 ∷ ∀ o a1 a2 r1 r2 r3
            . o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff2r3 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff3r3 ∷ ∀ o a1 a2 a3 r1 r2 r3
            . o → Method → a1 → a2 → a3 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff3r3 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff4r3
  ∷ ∀ o a1 a2 a3 a4 r1 r2 r3
  . o → Method → a1 → a2 → a3 → a4 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff4r3 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEffect $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)


callAff0r4
  ∷ ∀ o r1 r2 r3 r4
  . o → Method → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff0r4 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEffect $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff1r4
  ∷ ∀ o a1 r1 r2 r3 r4
  . o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff1r4 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEffect $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff2r4
  ∷ ∀ o a1 a2 r1 r2 r3 r4
  . o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff2r4 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEffect $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff3r4
  ∷ ∀ o a1 a2 a3 r1 r2 r3 r4
  . o → Method → a1 → a2 → a3
  → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff3r4 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEffect $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff4r4
  ∷ ∀ o a1 a2 a3 a4 r1 r2 r3 r4
  . o → Method → a1 → a2 → a3 → a4
  → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff4r4 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEffect $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)




callAff0r1' ∷ ∀ o c. o → Method → Aff c
callAff0r1' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn1 \res →
    unsafePerformEffect $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff1r1' ∷ ∀ o a1 c. o → Method → a1 → Aff c
callAff1r1' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn1 \res →
    unsafePerformEffect $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff2r1' ∷ ∀ o a1 a2 c. o → Method → a1 → a2 → Aff c
callAff2r1' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn1 \res →
    unsafePerformEffect $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff3r1' ∷ ∀ o a1 a2 a3 c. o → Method → a1 → a2 → a3 → Aff c
callAff3r1' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn1 \res →
    unsafePerformEffect $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff4r1' ∷ ∀ o a1 a2 a3 a4 c. o → Method → a1 → a2 → a3 → a4 → Aff c
callAff4r1' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn1 \res →
    unsafePerformEffect $ maybe (cb $ Right res) (cb <<< Left) (mkError res)


callAff0r2' ∷ ∀ o r1 r2. o → Method → Aff {res1 ∷ r1, res2 ∷ r2}
callAff0r2' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn2 \res1 res2 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff1r2' ∷ ∀ o a1 r1 r2. o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff1r2' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn2 \res1 res2 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff2r2'
  ∷ ∀ o a1 a2 r1 r2. o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2}
callAff2r2' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn2 \res1 res2 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff3r2'
  ∷ ∀ o a1 a2 a3 r1 r2. o → Method → a1 → a2 → a3
  → Aff {res1 ∷ r1, res2 ∷ r2}
callAff3r2' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn2 \res1 res2 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff4r2'
  ∷ ∀ o a1 a2 a3 a4 r1 r2. o → Method → a1 → a2 → a3 → a4
  → Aff {res1 ∷ r1, res2 ∷ r2}
callAff4r2' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \res1 res2 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)


callAff0r3'
  ∷ ∀ o r1 r2 r3. o → Method → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff0r3' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn3 \res1 res2 res3 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff1r3'
  ∷ ∀ o a1 r1 r2 r3. o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff1r3' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn3 \res1 res2 res3 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff2r3'
  ∷ ∀ o a1 a2 r1 r2 r3
  . o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff2r3' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn3 \res1 res2 res3 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff3r3'
  ∷ ∀ o a1 a2 a3 r1 r2 r3
  . o → Method → a1 → a2 → a3 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff3r3' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn3 \res1 res2 res3 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff4r3'
  ∷ ∀ o a1 a2 a3 a4 r1 r2 r3
  . o → Method → a1 → a2 → a3 → a4 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff4r3' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \res1 res2 res3 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)


callAff0r4'
  ∷ ∀ o r1 r2 r3 r4
  . o → Method → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff0r4' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff1r4'
  ∷ ∀ o a1 r1 r2 r3 r4
  . o → Method → a1 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff1r4' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff2r4'
  ∷ ∀ o a1 a2 r1 r2 r3 r4
  . o → Method → a1 → a2 → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff2r4' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff3r4'
  ∷ ∀ o a1 a2 a3 r1 r2 r3 r4
  . o → Method → a1 → a2 → a3
  → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff3r4' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff4r4'
  ∷ ∀ o a1 a2 a3 a4 r1 r2 r3 r4
  . o → Method → a1 → a2 → a3 → a4
  → Aff {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff4r4' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEffect $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)




listen0 ∷ ∀ o . o → Method → (Unit → Unit) → Effect Unit
listen0 o m g = callEffect1 o m (mkFn0 g)

listen1 ∷ ∀ o a1. o → Method → (a1 → Unit) → Effect Unit
listen1 o m g = callEffect1 o m (mkFn1 g)

listen2 ∷ ∀ o a1 a2. o → Method → (a1 → a2 → Unit) → Effect Unit
listen2 o m g = callEffect1 o m (mkFn2 g)

listen3 ∷ ∀ o a1 a2 a3. o → Method → (a1 → a2 → a3 → Unit) → Effect Unit
listen3 o m g = callEffect1 o m (mkFn3 g)

listen4 ∷ ∀ o a1 a2 a3 a4. o → Method → (a1 → a2 → a3 → a4 → Unit) → Effect Unit
listen4 o m g = callEffect1 o m (mkFn4 g)


listenTo0 ∷ ∀ o . o → Method → Event → (Unit → Unit) → Effect Unit
listenTo0 o m e g = callEffect2 o m e (mkFn0 g)

listenTo1 ∷ ∀ o a1. o → Method → Event → (a1 → Unit) → Effect Unit
listenTo1 o m e g = callEffect2 o m e (mkFn1 g)

listenTo2 ∷ ∀ o a1 a2. o → Method → Event → (a1 → a2 → Unit) → Effect Unit
listenTo2 o m e g = callEffect2 o m e (mkFn2 g)

listenTo3
  ∷ ∀ o a1 a2 a3. o → Method → Event → (a1 → a2 → a3 → Unit) → Effect Unit
listenTo3 o m e g = callEffect2 o m e (mkFn3 g)

listenTo4
  ∷ ∀ o a1 a2 a3 a4. o → Method → Event → (a1 → a2 → a3 → a4 → Unit)
  → Effect Unit
listenTo4 o m e g = callEffect2 o m e (mkFn4 g)


listenToEffect0 ∷ ∀ o . o → Method → Event → (Unit → Effect Unit) → Effect Unit
listenToEffect0 o m e g = callEffect2 o m e (mkEffectFn1 g)

listenToEffect1 ∷ ∀ o a1. o → Method → Event → (a1 → Effect Unit) → Effect Unit
listenToEffect1 o m e g = callEffect2 o m e (mkEffectFn1 g)

listenToEffect2
  ∷ ∀ o a1 a2. o → Method → Event → (a1 → a2 → Effect Unit) → Effect Unit
listenToEffect2 o m e g = callEffect2 o m e (mkEffectFn2 g)

listenToEffect3
  ∷ ∀ o a1 a2 a3. o → Method → Event → (a1 → a2 → a3 → Effect Unit) → Effect Unit
listenToEffect3 o m e g = callEffect2 o m e (mkEffectFn3 g)

listenToEffect4
  ∷ ∀ o a1 a2 a3 a4. o → Method → Event → (a1 → a2 → a3 → a4 → Effect Unit)
  → Effect Unit
listenToEffect4 o m e g = callEffect2 o m e (mkEffectFn4 g)
