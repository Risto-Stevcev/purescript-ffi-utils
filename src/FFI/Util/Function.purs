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

  , callEff0
  , callEff1
  , callEff2
  , callEff3
  , callEff4
  , callEff5
  , callEff6

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

import Control.Category ((<<<))
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn10, EffFn2, EffFn5, EffFn6, EffFn7, EffFn8, EffFn9, mkEffFn1, mkEffFn2, mkEffFn3, mkEffFn4, runEffFn10, runEffFn2, runEffFn5, runEffFn6, runEffFn7, runEffFn8, runEffFn9)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
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


foreign import _callEff0 ∷ ∀ eff o b. EffFn2 eff o Method b
foreign import _callEff1
  ∷ ∀ eff m o a1 b. EffFn5 eff (m → Maybe m) (Maybe m) o Method a1 b
foreign import _callEff2
  ∷ ∀ eff m o a1 a2 b. EffFn6 eff (m → Maybe m) (Maybe m) o Method a1 a2 b
foreign import _callEff3
  ∷ ∀ eff m o a1 a2 a3 b. EffFn7 eff (m → Maybe m) (Maybe m) o Method a1 a2 a3 b
foreign import _callEff4
  ∷ ∀ eff m o a1 a2 a3 a4 b. EffFn8 eff (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 b
foreign import _callEff5
  ∷ ∀ eff m o a1 a2 a3 a4 a5 b. EffFn9 eff (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 b
foreign import _callEff6
  ∷ ∀ eff m o a1 a2 a3 a4 a5 a6 b. EffFn10 eff (m → Maybe m) (Maybe m) o Method a1 a2 a3 a4 a5 a6 b

callEff0 ∷ ∀ o eff b. o → Method → Eff eff b
callEff0 = runEffFn2 _callEff0

callEff1 ∷ ∀ o a1 eff b. o → Method → a1 → Eff eff b
callEff1 = runEffFn5 _callEff1 Just Nothing

callEff2 ∷ ∀ o a1 a2 eff b. o → Method → a1 → a2 → Eff eff b
callEff2 = runEffFn6 _callEff2 Just Nothing

callEff3 ∷ ∀ o a1 a2 a3 eff b. o → Method → a1 → a2 → a3 → Eff eff b
callEff3 = runEffFn7 _callEff3 Just Nothing

callEff4 ∷ ∀ o a1 a2 a3 a4 eff b. o → Method → a1 → a2 → a3 → a4 → Eff eff b
callEff4 = runEffFn8 _callEff4 Just Nothing

callEff5
  ∷ ∀ o a1 a2 a3 a4 a5 eff b. o → Method → a1 → a2 → a3 → a4 → a5 → Eff eff b
callEff5 = runEffFn9 _callEff5 Just Nothing

callEff6
  ∷ ∀ o a1 a2 a3 a4 a5 a6 eff b
  . o → Method → a1 → a2 → a3 → a4 → a5 → a6 → Eff eff b
callEff6 = runEffFn10 _callEff6 Just Nothing



callAff0r1 ∷ ∀ o b c. o → Method → Aff b c
callAff0r1 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn2 \err res →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff1r1 ∷ ∀ o a1 b c. o → Method → a1 → Aff b c
callAff1r1 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn2 \err res →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff2r1 ∷ ∀ o a1 a2 b c. o → Method → a1 → a2 → Aff b c
callAff2r1 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn2 \err res →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff3r1 ∷ ∀ o a1 a2 a3 b c. o → Method → a1 → a2 → a3 → Aff b c
callAff3r1 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn2 \err res →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)

callAff4r1 ∷ ∀ o a1 a2 a3 a4 b c. o → Method → a1 → a2 → a3 → a4 → Aff b c
callAff4r1 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \err res →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right res)
      else (cb $ Left err)


callAff0r2 ∷ ∀ o b r1 r2. o → Method → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff0r2 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn3 \err res1 res2 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff1r2 ∷ ∀ o a1 b r1 r2. o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff1r2 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn3 \err res1 res2 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff2r2
  ∷ ∀ o a1 a2 b r1 r2. o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff2r2 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn3 \err res1 res2 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff3r2
  ∷ ∀ o a1 a2 a3 b r1 r2
  . o → Method → a1 → a2 → a3 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff3r2 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn3 \err res1 res2 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)

callAff4r2
  ∷ ∀ o a1 a2 a3 a4 b r1 r2
  . o → Method → a1 → a2 → a3 → a4 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff4r2 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \err res1 res2 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2})
      else (cb $ Left err)


callAff0r3 ∷ ∀ o b r1 r2 r3
            . o → Method → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff0r3 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn4 \err res1 res2 res3 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff1r3 ∷ ∀ o a1 b r1 r2 r3
            . o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff1r3 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff2r3 ∷ ∀ o a1 a2 b r1 r2 r3
            . o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff2r3 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff3r3 ∷ ∀ o a1 a2 a3 b r1 r2 r3
            . o → Method → a1 → a2 → a3 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff3r3 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)

callAff4r3
  ∷ ∀ o a1 a2 a3 a4 b r1 r2 r3
  . o → Method → a1 → a2 → a3 → a4 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff4r3 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \err res1 res2 res3 →
    unsafePerformEff $
      if (isNullOrUndefined err)
      then (cb $ Right {res1: res1, res2: res2, res3: res3})
      else (cb $ Left err)


callAff0r4
  ∷ ∀ o b r1 r2 r3 r4
  . o → Method → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff0r4 o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEff $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff1r4
  ∷ ∀ o a1 b r1 r2 r3 r4
  . o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff1r4 o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEff $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff2r4
  ∷ ∀ o a1 a2 b r1 r2 r3 r4
  . o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff2r4 o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEff $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff3r4
  ∷ ∀ o a1 a2 a3 b r1 r2 r3 r4
  . o → Method → a1 → a2 → a3
  → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff3r4 o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEff $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)


callAff4r4
  ∷ ∀ o a1 a2 a3 a4 b r1 r2 r3 r4
  . o → Method → a1 → a2 → a3 → a4
  → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff4r4 o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn5 \err res1 res2 res3 res4 → unsafePerformEff $
    if (isNullOrUndefined err)
    then (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
    else (cb $ Left err)




callAff0r1' ∷ ∀ o b c. o → Method → Aff b c
callAff0r1' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn1 \res →
    unsafePerformEff $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff1r1' ∷ ∀ o a1 b c. o → Method → a1 → Aff b c
callAff1r1' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn1 \res →
    unsafePerformEff $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff2r1' ∷ ∀ o a1 a2 b c. o → Method → a1 → a2 → Aff b c
callAff2r1' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn1 \res →
    unsafePerformEff $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff3r1' ∷ ∀ o a1 a2 a3 b c. o → Method → a1 → a2 → a3 → Aff b c
callAff3r1' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn1 \res →
    unsafePerformEff $ maybe (cb $ Right res) (cb <<< Left) (mkError res)

callAff4r1' ∷ ∀ o a1 a2 a3 a4 b c. o → Method → a1 → a2 → a3 → a4 → Aff b c
callAff4r1' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn1 \res →
    unsafePerformEff $ maybe (cb $ Right res) (cb <<< Left) (mkError res)


callAff0r2' ∷ ∀ o b r1 r2. o → Method → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff0r2' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn2 \res1 res2 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff1r2' ∷ ∀ o a1 b r1 r2. o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff1r2' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn2 \res1 res2 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff2r2'
  ∷ ∀ o a1 a2 b r1 r2. o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff2r2' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn2 \res1 res2 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff3r2'
  ∷ ∀ o a1 a2 a3 b r1 r2. o → Method → a1 → a2 → a3
  → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff3r2' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn2 \res1 res2 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)

callAff4r2'
  ∷ ∀ o a1 a2 a3 a4 b r1 r2. o → Method → a1 → a2 → a3 → a4
  → Aff b {res1 ∷ r1, res2 ∷ r2}
callAff4r2' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn2 \res1 res2 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2})
      (cb <<< Left)
      (mkError res1)


callAff0r3'
  ∷ ∀ o b r1 r2 r3. o → Method → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff0r3' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn3 \res1 res2 res3 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff1r3'
  ∷ ∀ o a1 b r1 r2 r3. o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff1r3' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn3 \res1 res2 res3 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff2r3'
  ∷ ∀ o a1 a2 b r1 r2 r3
  . o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff2r3' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn3 \res1 res2 res3 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff3r3'
  ∷ ∀ o a1 a2 a3 b r1 r2 r3
  . o → Method → a1 → a2 → a3 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff3r3' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn3 \res1 res2 res3 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)

callAff4r3'
  ∷ ∀ o a1 a2 a3 a4 b r1 r2 r3
  . o → Method → a1 → a2 → a3 → a4 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3}
callAff4r3' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn3 \res1 res2 res3 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3})
      (cb <<< Left)
      (mkError res1)


callAff0r4'
  ∷ ∀ o b r1 r2 r3 r4
  . o → Method → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff0r4' o m = makeAff $ \cb →
  pure $ call1 o m $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff1r4'
  ∷ ∀ o a1 b r1 r2 r3 r4
  . o → Method → a1 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff1r4' o m a1 = makeAff $ \cb →
  pure $ call2 o m a1 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff2r4'
  ∷ ∀ o a1 a2 b r1 r2 r3 r4
  . o → Method → a1 → a2 → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff2r4' o m a1 a2 = makeAff $ \cb →
  pure $ call3 o m a1 a2 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff3r4'
  ∷ ∀ o a1 a2 a3 b r1 r2 r3 r4
  . o → Method → a1 → a2 → a3
  → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff3r4' o m a1 a2 a3 = makeAff $ \cb →
  pure $ call4 o m a1 a2 a3 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)


callAff4r4'
  ∷ ∀ o a1 a2 a3 a4 b r1 r2 r3 r4
  . o → Method → a1 → a2 → a3 → a4
  → Aff b {res1 ∷ r1, res2 ∷ r2, res3 ∷ r3, res4 ∷ r4}
callAff4r4' o m a1 a2 a3 a4 = makeAff $ \cb →
  pure $ call5 o m a1 a2 a3 a4 $ mkFn4 \res1 res2 res3 res4 →
    unsafePerformEff $
      maybe
      (cb $ Right {res1: res1, res2: res2, res3: res3, res4: res4})
      (cb <<< Left)
      (mkError res1)




listen0 ∷ ∀ o e. o → Method → (Unit → Unit) → Eff e Unit
listen0 o m g = callEff1 o m (mkFn0 g)

listen1 ∷ ∀ o e a1. o → Method → (a1 → Unit) → Eff e Unit
listen1 o m g = callEff1 o m (mkFn1 g)

listen2 ∷ ∀ o e a1 a2. o → Method → (a1 → a2 → Unit) → Eff e Unit
listen2 o m g = callEff1 o m (mkFn2 g)

listen3 ∷ ∀ o e a1 a2 a3. o → Method → (a1 → a2 → a3 → Unit) → Eff e Unit
listen3 o m g = callEff1 o m (mkFn3 g)

listen4 ∷ ∀ o e a1 a2 a3 a4. o → Method → (a1 → a2 → a3 → a4 → Unit) → Eff e Unit
listen4 o m g = callEff1 o m (mkFn4 g)


listenTo0 ∷ ∀ o e. o → Method → Event → (Unit → Unit) → Eff e Unit
listenTo0 o m e g = callEff2 o m e (mkFn0 g)

listenTo1 ∷ ∀ o e a1. o → Method → Event → (a1 → Unit) → Eff e Unit
listenTo1 o m e g = callEff2 o m e (mkFn1 g)

listenTo2 ∷ ∀ o e a1 a2. o → Method → Event → (a1 → a2 → Unit) → Eff e Unit
listenTo2 o m e g = callEff2 o m e (mkFn2 g)

listenTo3
  ∷ ∀ o e a1 a2 a3. o → Method → Event → (a1 → a2 → a3 → Unit) → Eff e Unit
listenTo3 o m e g = callEff2 o m e (mkFn3 g)

listenTo4
  ∷ ∀ o e a1 a2 a3 a4. o → Method → Event → (a1 → a2 → a3 → a4 → Unit)
  → Eff e Unit
listenTo4 o m e g = callEff2 o m e (mkFn4 g)


listenToEff0 ∷ ∀ o e. o → Method → Event → (Unit → Eff e Unit) → Eff e Unit
listenToEff0 o m e g = callEff2 o m e (mkEffFn1 g)

listenToEff1 ∷ ∀ o e a1. o → Method → Event → (a1 → Eff e Unit) → Eff e Unit
listenToEff1 o m e g = callEff2 o m e (mkEffFn1 g)

listenToEff2
  ∷ ∀ o e a1 a2. o → Method → Event → (a1 → a2 → Eff e Unit) → Eff e Unit
listenToEff2 o m e g = callEff2 o m e (mkEffFn2 g)

listenToEff3
  ∷ ∀ o e a1 a2 a3. o → Method → Event → (a1 → a2 → a3 → Eff e Unit) → Eff e Unit
listenToEff3 o m e g = callEff2 o m e (mkEffFn3 g)

listenToEff4
  ∷ ∀ o e a1 a2 a3 a4. o → Method → Event → (a1 → a2 → a3 → a4 → Eff e Unit)
  → Eff e Unit
listenToEff4 o m e g = callEff2 o m e (mkEffFn4 g)
