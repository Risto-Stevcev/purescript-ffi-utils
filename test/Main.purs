module Test.Main where

import Prelude (Unit, class Show, show, bind, discard, ($), (#), (<$>), (>>=), (==), (<>), pure, unit, const)
import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Control.Monad.Aff (Aff, launchAff, liftEff', Canceler)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log) as AffLog
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Control.Coroutine (Consumer, Producer, Process, ($$), runProcess, await)
import Control.Coroutine.Aff (produce)
import FFI.Util (parseOptions, require, stringify, typeof, instanceof, property', property, setProperty, global)
import FFI.Util.Log (logAny)
import FFI.Util.Class (class Taggable, class Untaggable, untag, tag)
import FFI.Util.Function (callEff0, callEff1, callAff2r1, listenToEff0)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Foreign (F, toForeign, unsafeFromForeign)
import Data.Foreign.Class (class Decode, decode)


-- | Define some data types for the FFI libraries
foreign import data FileSystemM :: Type
foreign import data Stream :: Type
foreign import data FS :: Effect   -- To keep track of file system side effects

foreign import data BufferM :: Type
foreign import data Buffer :: Type
foreign import data BUFFER :: Effect  -- To keep track of buffer side effects

-- | We can require() modules directly and easily
-- | As a convention, module data types are suffixed with M
fs :: FileSystemM
fs = require "fs"

buffer :: BufferM
buffer = require "buffer"

-- | It becomes easy to convert a JS function into a PS one
-- | From the 'buffer' module:
toBuffer :: forall e. String -> Eff (err :: EXCEPTION, buffer :: BUFFER | e) Buffer
toBuffer s = callEff1 buffer "Buffer" s

toString :: forall e. Buffer -> Eff (err :: EXCEPTION, buffer :: BUFFER | e) String
toString b = callEff0 b "toString"

-- | From the 'fs' module:
readFile :: forall e. String -> Aff (fs :: FS | e) String
readFile file = callAff2r1 fs "readFile" file "utf8"

createReadStream :: forall e. String -> Eff (err :: EXCEPTION, fs :: FS | e) Stream
createReadStream file = callEff1 fs "createReadStream" file


-- | Signatures in JS are often untagged sums. Here are some helper classes to both tag and untag tagged sums
-- | in Purescript for use with the FFI
data Primitive = S String | N Number | B Boolean

instance tagPrimitive :: Taggable Primitive where
  tag a | typeof a == "string"  = S (unsafeFromForeign a)
        | typeof a == "number"  = N (unsafeFromForeign a)
        | typeof a == "boolean" = B (unsafeFromForeign a)
  tag _ = S ""

instance untagPrimitive :: Untaggable Primitive where
  untag (S a) = toForeign a
  untag (N a) = toForeign a
  untag (B a) = toForeign a

instance isForeignFoo :: Decode Primitive where
  decode value = do
    let string = S <$> (decode value :: F String)
    let num    = N <$> (decode value :: F Number)
    let bool   = B <$> (decode value :: F Boolean)
    string <|> num <|> bool

instance showPrimitive :: Show Primitive where
  show (S a) = "S " <> (show a)
  show (N a) = "N " <> (show a)
  show (B a) = "B " <> (show a)


-- | We can have objects that use Maybe types to convey optional values
type SomeConfigObject = { foo :: String, bar :: { baz :: Maybe Int, qux :: Boolean } }

config1 :: SomeConfigObject
config1 = { foo: "bar", bar: { baz: Nothing, qux: true } }


config2 :: SomeConfigObject
config2 = { foo: "bar", bar: { baz: Just 3, qux: true } }


-- | We can easily listen to events
-- | In this example coroutines are used to encapsulate the set of events for Streams
data StreamEvent = Readable | Data | End

streamProducer :: forall e. Stream -> Producer StreamEvent (Aff (avar :: AVAR | e)) Unit
streamProducer stream = produce \emit -> do
  listenToEff0 stream "on" "readable" \_ -> emit (Left Readable)
  listenToEff0 stream "on" "data"     \_ -> emit (Left Data)
  listenToEff0 stream "on" "end"      \_ -> do
    emit (Left End)
    emit (Right unit)

streamConsumer :: forall e. Consumer StreamEvent (Aff (console :: CONSOLE | e)) Unit
streamConsumer = forever do
  e <- await
  case e of
    Readable -> lift $ AffLog.log "Readable event fired"
    Data     -> lift $ AffLog.log "Data event fired"
    End      -> lift $ AffLog.log "End event fired"


streamProcess :: forall e. Stream -> Process (Aff (console :: CONSOLE, avar :: AVAR | e)) Unit
streamProcess stream = (streamProducer stream) $$ (streamConsumer)


main :: forall e
      . Eff (fs :: FS, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER, err :: EXCEPTION, exception :: EXCEPTION | e)
        (Canceler (fs :: FS, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER, err :: EXCEPTION | e))
main = do
  log $ stringify false $ parseOptions config1  -- {"foo":"bar","bar":{"baz":null,"qux":true}}
  log $ stringify false $ parseOptions config2  -- {"foo":"bar","bar":{"baz":3,"qux":true}}

  -- | Show the result of untagging a tagged sum using Taggable, Untaggable and Decode
  let pi = N 3.1415
  log $ show pi      -- N 3.1415
  logAny $ untag pi  -- 3.1415
  log $ show $ (untag pi # tag) :: Primitive  -- N 3.1415
  log $ show $ runExcept $ (untag pi # decode) :: F Primitive  -- (Right N 3.1415)

  -- | Setting a property
  pure $ setProperty global "foo" "bar"
  log $ (property global "foo") :: String  -- bar

  -- | Checking a type
  log $ show $ [1,2,3] `instanceof` (property' "Array")

  -- | Demonstrates toBuffer and toString
  toBuffer "foobar" >>= \buf -> do
    log $ show $ buf `instanceof` (property buffer "Buffer")  -- true
    logAny buf            -- <Buffer 66 6f 6f 62 61 72>
    buf' <- toString buf  -- foobar
    log buf'

  -- | Launch the stream coroutine
  _ <- launchAff $ do
    stream <- liftEff' $ createReadStream "/home/pureuser/src/bower.json"
    either (const $ pure unit) (\s -> runProcess (streamProcess s)) stream

  -- | Outputs the contents of bower.json to stdout
  launchAff $ do
    contents <- readFile "/home/pureuser/src/bower.json"
    AffLog.log contents
