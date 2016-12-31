module Test.Main where

import Prelude (Unit, bind, ($), (>>=), pure, unit, const)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Aff (Aff, launchAff, liftEff', Canceler)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (log) as AffLog
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Control.Coroutine (Consumer, Producer, Process, ($$), runProcess, await)
import Control.Coroutine.Aff (produce)
import FFI.Util (parseOptions, require, stringify)
import FFI.Util.Log (logAny)
import FFI.Util.Function (call0, call1, callAff2r1, listenToEff0)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)


-- | Define some data types for the FFI libraries
foreign import data FileSystemM :: *
foreign import data Stream :: *
foreign import data FS :: !   -- To keep track of file system side effects

foreign import data BufferM :: *
foreign import data Buffer :: *
foreign import data BUFFER :: !  -- To keep track of buffer side effects

-- | We can require() modules directly and easily
-- | As a convention, module data types are suffixed with M
fs :: FileSystemM
fs = require "fs"

buffer :: BufferM
buffer = require "buffer"

-- | It becomes easy to convert a JS function into a PS one
-- | From the 'buffer' module:
toBuffer :: forall e. String -> Eff (err :: EXCEPTION, buffer :: BUFFER | e) Buffer
toBuffer s = pure $ call1 buffer "Buffer" s

toString :: forall e. Buffer -> Eff (err :: EXCEPTION, buffer :: BUFFER | e) String
toString b = pure $ call0 b "toString"

-- | From the 'fs' module:
readFile :: forall e. String -> Aff (fs :: FS | e) String
readFile file = callAff2r1 fs "readFile" file "utf8"

createReadStream :: forall e. String -> Eff (err :: EXCEPTION, fs :: FS | e) Stream
createReadStream file = pure $ call1 fs "createReadStream" file



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
streamProducer zz = produce \emit -> do
  listenToEff0 zz "on" "readable" \_ -> emit (Left Readable)
  listenToEff0 zz "on" "data"     \_ -> emit (Left Data)
  listenToEff0 zz "on" "end"      \_ -> do
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
      . Eff (fs :: FS, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER, err :: EXCEPTION | e)
        (Canceler (fs :: FS, console :: CONSOLE, avar :: AVAR, buffer :: BUFFER | e))
main = do
  log $ stringify false $ parseOptions config1  -- {"foo":"bar","bar":{"qux":true}}
  log $ stringify false $ parseOptions config2  -- {"foo":"bar","bar":{"baz":3,"qux":true}}

  toBuffer "foobar" >>= \buf -> do
    logAny buf            -- <Buffer 66 6f 6f 62 61 72>
    buf' <- toString buf  -- foobar
    log buf'

  -- | Launch the stream coroutine
  launchAff $ do
    stream <- liftEff' $ createReadStream "/home/pureuser/src/bower.json"
    either (const $ pure unit) (\s -> runProcess (streamProcess s)) stream

  -- | Outputs the contents of bower.json to stdout
  launchAff $ do
    contents <- readFile "/home/pureuser/src/bower.json"
    AffLog.log contents
