module Test.FFI.Utils (TestEffects, Buffer, FS, BUFFER, main) where

import Prelude

import Control.Alt ((<|>))
import Control.Coroutine (Consumer, Producer, Process, ($$), runProcess, await)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import FFI.Util (parseOptions, require, stringify, typeof, instanceof, property', property, propertyPath, setProperty, global)
import FFI.Util.Class (class Taggable, class Untaggable, tag, untag)
import FFI.Util.Function (callAff2r1, callEff0, callEff1, listenToEff0, listenToEff1)
import Node.Process (PROCESS, cwd)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)



-- | Define some data types for the FFI libraries
foreign import data FileSystemM ∷ Type
foreign import data Stream ∷ Type
foreign import data FS ∷ Effect   -- To keep track of file system side effects

foreign import data BufferM ∷ Type
foreign import data Buffer ∷ Type
foreign import data BUFFER ∷ Effect  -- To keep track of buffer side effects

type BufferEffects e = (err ∷ EXCEPTION, buffer ∷ BUFFER | e)

-- | require() modules directly and easily
-- | As a convention, module data types are suffixed with M
fs ∷ FileSystemM
fs = require "fs"

-- | From the 'fs' module:
readFile ∷ ∀ e. String → Aff (fs ∷ FS | e) String
readFile file = callAff2r1 fs "readFile" file "utf8"

createReadStream ∷ ∀ e. String → Eff (err ∷ EXCEPTION, fs ∷ FS | e) Stream
createReadStream file = callEff1 fs "createReadStream" file


-- | These are some helper classes to conveniently tag and untag tagged sums
-- | for the FFI
data Primitive = S String | N Number | B Boolean

instance eqPrimitive ∷ Eq Primitive where
  eq (S s) (S s') = s == s'
  eq (N n) (N n') = n == n'
  eq (B b) (B b') = b == b'
  eq _ _ = false

instance tagPrimitive ∷ Taggable Primitive where
  tag a | typeof a == "string"  = S (unsafeFromForeign a)
        | typeof a == "number"  = N (unsafeFromForeign a)
        | typeof a == "boolean" = B (unsafeFromForeign a)
  tag _ = S ""

instance untagPrimitive ∷ Untaggable Primitive where
  untag (S a) = toForeign a
  untag (N a) = toForeign a
  untag (B a) = toForeign a

instance isForeignFoo ∷ Decode Primitive where
  decode value = do
    let string = S <$> (decode value ∷ F String)
    let num    = N <$> (decode value ∷ F Number)
    let bool   = B <$> (decode value ∷ F Boolean)
    string <|> num <|> bool

instance showPrimitive ∷ Show Primitive where
  show (S a) = "S " <> (show a)
  show (N a) = "N " <> (show a)
  show (B a) = "B " <> (show a)


-- | Config objects can have `Maybe a` types to convey optional values, which
-- | will automatically be flattened to `null` or `a` using `parseOptions`
-- | for the FFI
type SomeConfigObject = { foo ∷ String, bar ∷ { baz ∷ Maybe Int, qux ∷ Boolean } }

type StreamRef = STRef Buffer (Array Buffer)

data StreamEvent = Readable | Data Buffer | End


-- | Coroutines are useful for streams that emit chunks of data before closing,
-- | such as the Stream returned by createReadStream
streamProducer ∷ ∀ e. Stream → Producer StreamEvent (Aff (avar ∷ AVAR, st ∷ ST Buffer | e)) Unit
streamProducer stream = produce \emit → do
  listenToEff0 stream "on" "readable" \_ → emit (Left Readable)
  listenToEff1 stream "on" "data"     \(d ∷ Buffer) → emit (Left $ Data d)
  listenToEff0 stream "on" "end"      \_ → do
    emit (Left End)
    emit (Right unit)

streamConsumer ∷ ∀ e. StreamRef → Consumer StreamEvent (Aff (st ∷ ST Buffer | e)) Unit
streamConsumer ref = forever do
  e ← await
  liftEff $ case e of
    Readable → pure unit
    Data buf → (pure unit) <* modifySTRef ref (_ <> [buf])
    End      → pure unit

streamProcess ∷ ∀ e. Stream → StreamRef → Process (Aff (console ∷ CONSOLE, avar ∷ AVAR, st ∷ ST Buffer | e)) Unit
streamProcess stream ref = (streamProducer stream) $$ (streamConsumer ref)

decodePrimitive ∷ ∀ a. Decode a ⇒ Foreign → Either (NonEmptyList ForeignError) a
decodePrimitive = runExcept <<< decode

untagTag ∷ ∀ a. Taggable a ⇒ Untaggable a ⇒ a → a
untagTag = tag <<< untag

type TestEffects e =
  ( fs ∷ FS, console ∷ CONSOLE, avar ∷ AVAR, buffer ∷ BUFFER
  , err ∷ EXCEPTION, process ∷ PROCESS, st ∷ ST Buffer
  | e
  )


main ∷ ∀ e. Eff (RunnerEffects (TestEffects e)) Unit
main = run [consoleReporter] do
  describe "purescript-ffi-utils" do
    describe "parseOptions" do
      let config1 = { foo: "bar"
                    , bar: { baz: Nothing, qux: true }
                    } ∷ SomeConfigObject

      let config2 = { foo: "bar"
                    , bar: { baz: Just 3, qux: true }
                    } ∷ SomeConfigObject

      let parseStringify = stringify false <<< parseOptions

      it "should correctly parse objects containing Maybe as a type" do
        parseStringify config1 `shouldEqual`
          "{\"foo\":\"bar\",\"bar\":{\"baz\":null,\"qux\":true}}"

        parseStringify config2 `shouldEqual`
          "{\"foo\":\"bar\",\"bar\":{\"baz\":3,\"qux\":true}}"


    describe "Taggable/Untaggable" do
      it "should tag/untag a variant" do
        let pi = N 3.1415
        (decodePrimitive $ untag pi) `shouldEqual` (Right 3.1415)
        untagTag pi `shouldEqual` pi


    describe "property/setProperty" do
      it "should set and get a property for an object" do
        pure $ setProperty global "foo" "bar"
        (property global "foo") ∷ String `shouldEqual` "bar"


    describe "propertyPath" do
      it "should get the deeply nested property" do
        propertyPath {x: {y: {z: 3}}} ["x", "y", "z"] `shouldEqual` 3


    describe "instanceof" do
      it "should check whether the value is an intanceof the given object" do
        [1,2,3] `instanceof` (property' "Array") `shouldEqual` true


    describe "callEff*" do
      it "should call the corresponding FFI functions with the given args" do
        let bufferM = (require "buffer") ∷ BufferM
        let toBuffer (string ∷ String) = (callEff1 bufferM "Buffer" string)
                                       ∷ ∀ eff. Eff (BufferEffects eff) Buffer
        let toString (buffer ∷ Buffer) = (callEff0 buffer "toString")
                                       ∷ ∀ eff. Eff (BufferEffects eff) String

        foo ← liftEff $ toBuffer "foobar"
        foo `instanceof` (property bufferM "Buffer") `shouldEqual` true
        foo' ← liftEff $ toString foo
        foo' `shouldEqual` "foobar"


    describe "callAff*" do
      it "should call the corresponding FFI async function returning Aff" do
        let bufferM = (require "buffer") ∷ BufferM
        let concat (buffers ∷ Array Buffer) = (callEff1 (property bufferM "Buffer") "concat" buffers)
                                            ∷ ∀ eff. Eff (BufferEffects eff) Buffer
        let toString (buffer ∷ Buffer) = (callEff0 buffer "toString")
                                       ∷ ∀ eff. Eff (BufferEffects eff) String
        let bowerFile = unsafePerformEff $ (_ <> "/bower.json") <$> cwd

        -- | Launch the stream coroutine
        ref ← liftEff $ newSTRef []
        stream ← liftEff' $ createReadStream bowerFile
        runProcess (streamProcess stream ref)

        expected ← liftEff $ (toString <=< concat <=< readSTRef) ref
        actual ← readFile bowerFile

        expected `shouldEqual` (actual <> "")
