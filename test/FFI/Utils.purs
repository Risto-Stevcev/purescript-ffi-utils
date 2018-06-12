module Test.FFI.Utils (Buffer, main) where

import Prelude

import Control.Alt ((<|>))
import Control.Coroutine (Consumer, Producer, Process, ($$), runProcess, await)
import Control.Coroutine.Aff (produce)
import Effect.Aff (Aff, liftEffect')
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef)
import Data.Either (Either(..))
import Foreign (F, Foreign, ForeignError, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import FFI.Util (parseOptions, require, stringify, typeof, instanceof, property', property, propertyPath, setProperty, global)
import FFI.Util.Class (class Taggable, class Untaggable, tag, untag)
import FFI.Util.Function (callAff2r1, callEffect0, callEffect1, listenToEffect0, listenToEffect1)
import Node.Process (cwd)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)



-- | Define some data types for the FFI libraries
foreign import data FileSystemM ∷ Type
foreign import data Stream ∷ Type

foreign import data BufferM ∷ Type
foreign import data Buffer ∷ Type

-- | require() modules directly and easily
-- | As a convention, module data types are suffixed with M
fs ∷ FileSystemM
fs = require "fs"

-- | From the 'fs' module:
readFile ∷ String → Aff String
readFile file = callAff2r1 fs "readFile" file "utf8"

createReadStream ∷ String → Effect Stream
createReadStream file = callEffect1 fs "createReadStream" file


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
streamProducer ∷ Stream → Producer StreamEvent Aff Unit
streamProducer stream = produce \emit → do
  listenToEffect0 stream "on" "readable" \_ → emit (Left Readable)
  listenToEffect1 stream "on" "data"     \(d ∷ Buffer) → emit (Left $ Data d)
  listenToEffect0 stream "on" "end"      \_ → do
    emit (Left End)
    emit (Right unit)

streamConsumer ∷ StreamRef → Consumer StreamEvent Aff Unit
streamConsumer ref = forever do
  e ← await
  liftEffect $ case e of
    Readable → pure unit
    Data buf → (pure unit) <* modifySTRef ref (_ <> [buf])
    End      → pure unit

streamProcess ∷ Stream → StreamRef → Process Aff Unit
streamProcess stream ref = (streamProducer stream) $$ (streamConsumer ref)

decodePrimitive ∷ ∀ a. Decode a ⇒ Foreign → Either (NonEmptyList ForeignError) a
decodePrimitive = runExcept <<< decode

untagTag ∷ ∀ a. Taggable a ⇒ Untaggable a ⇒ a → a
untagTag = tag <<< untag


main ∷ Effect Unit
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


    describe "callEffect*" do
      it "should call the corresponding FFI functions with the given args" do
        let bufferM = (require "buffer") ∷ BufferM
        let toBuffer (string ∷ String) = (callEffect1 bufferM "Buffer" string)
                                       ∷ Effect Buffer
        let toString (buffer ∷ Buffer) = (callEffect0 buffer "toString")
                                       ∷ Effect String

        foo ← liftEffect $ toBuffer "foobar"
        foo `instanceof` (property bufferM "Buffer") `shouldEqual` true
        foo' ← liftEffect $ toString foo
        foo' `shouldEqual` "foobar"


    describe "callAff*" do
      it "should call the corresponding FFI async function returning Aff" do
        let bufferM = (require "buffer") ∷ BufferM
        let concat (buffers ∷ Array Buffer) = (callEffect1 (property bufferM "Buffer") "concat" buffers)
                                            ∷ Effect Buffer
        let toString (buffer ∷ Buffer) = (callEffect0 buffer "toString")
                                       ∷ Effect String
        let bowerFile = unsafePerformEffect $ (_ <> "/bower.json") <$> cwd

        -- | Launch the stream coroutine
        ref ← liftEffect $ newSTRef []
        stream ← liftEffect' $ createReadStream bowerFile
        runProcess (streamProcess stream ref)

        expected ← liftEffect $ (toString <=< concat <=< readSTRef) ref
        actual ← readFile bowerFile

        expected `shouldEqual` (actual <> "")
