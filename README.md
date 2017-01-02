# purescript-ffi-utils

A utility library for the purescript foreign function interface

This library is primarily intended to alleviate the unique challenge of javascript to purescript translation (and 
vice versa). Thus it is intended only for purescript's default javascript backend, and there are currently no 
intentions to support alternate backends.


## Accessing a module

You can access a module the same way you do in node:

```purescript
import FFI.Util (require)

-- Require the fs module
foreign import data FileSystemM :: *

fs :: FileSystemM
fs = require "fs"

-- Require the buffer module
foreign import data BufferM :: *

buffer :: BufferM
buffer = require "buffer"
```

Since the `require` function returns the module object, you likely should give it a new data type, suffixed here 
with M so that it's clearer that this is a module object.


## Calling a function

You can call functions using the `call0`, `call1`, etc, up to `call7` functions. The number refers to the number of 
arguments for that function. For each function, you need to pass the object and method to call first.

In this example, the `Buffer` function in the buffer module is called in order to create a new buffer. The `Buffer` 
function only takes one parameter, in this case a `String`, so it calls `call1`:

```purescript
import FFI.Util.Function (call1)

toBuffer :: String -> Buffer
toBuffer s = call1 buffer "Buffer" s
```

However there is an issue with this signature for `toBuffer`. Buffers in JS are mutable, so it makes sense to wrap 
it in `Eff` since they can be side effecting. This is as simple as changing the type signature to `Eff` and calling 
`pure` on the result:

```purescript
toBuffer :: forall e. String -> Eff (err :: EXCEPTION, buffer :: BUFFER | e) Buffer
toBuffer s = callEff1 buffer "Buffer" s
```


## Calling async functions

Calling an async function is similar to calling a regular function, except that it requires one extra step, which is to 
determine what the callback function returns. The async helper functions are written like `callAff2r1`, which means that 
the function takes two arguments (excluding the callback), and the callback **r**eturns one argument (excluding the 
error argument).

For example, for the `fs.readFile` function in the fs module, it takes the signature `fs.readFile(file[, options], callback)` 
and the callback is passed two arguments `(err, data)`, where `data` is the contents of the file, so it should use the 
`callAff2r1` helper function.

This implementation just passes a `String` for `options` to determine the output encoding of the file:

```purescript
import FFI.Util.Function (callAff2r1)

readFile :: forall e. String -> Aff (fs :: FS | e) String
readFile file = callAff2r1 fs "readFile" file "utf8"
```

JS has a convention where the first argument of a callback is a potential error, and the remaining arguments are the 
return values. The `callAff*` functions assume that this is the case. In some cases the format is a little different, 
where either an error is never returned, or the first argument is either an error or the result type. In those cases 
you should use the `callAff*'` versions that have a suffixed tick in the name.

For callbacks that return more than one argument, the result that Aff returns is in the form of 
`{ res1 :: r1, res2 :: r2, ...}` where res1 corresponds to the first argument, res2 for the second, and so on.


## Listeners

You can define listeners by using the functions that are in the form of `listen0`, `listenTo0`, and `listenToEff0`. 
The `listenTo` form takes an additional String for the listener event name, for JS callbacks like 

```js
someObject.on('eventname', function() { ... })
```

The `listenToEff` form returns the callback as an `Eff` function, which is convenient for defining coroutines. 
Coroutines are a nicer way of using listeners in purescript. See the library tests for an example on how to use them. 


## Optional function parameters

All of the call functions (ie. `call1` or `callAff2r1`) can take optional arguments in the form of `Maybe` types. If 
you pass in a `Maybe` type as a parameter to any of these functions, these functions will automatically handle the 
type so that `Just a` returns `a` and `Nothing` returns `null`, similar to how `toNullable` works from the 
purescript-nullable library.


## Properties

You can get properties using `property` and `propertyPath` and set properties with `setProperty` and `setPropertyPath`. 
There are two shortcut functions `property'` and `propertyPath'` which get a property from the `global` or `window` 
object (whichever is available).


## Tagged sums to untagged sums

Sometimes javascript functions arguments can be more than one type, often written in APIs like `String | Number`. 
However, these sum types are untagged, and in purescript you can only have tagged sums. There are convenience 
functions to untag a tagged sum for the JS FFI, or to tag an untagged sum from the JS FFI. See the library tests for 
an example on how to use them.


## Complementary libraries

There are other useful libraries for working with the FFI that complement this one, such as:

puescript-foreign, purescript-nullable, purescript-undefinable, purescript-aff, purescript-coroutines, 
purescript-aff-coroutines, purescript-functions, purescript-eff-functions, purescript-argonaut, purescript-generics, etc.
