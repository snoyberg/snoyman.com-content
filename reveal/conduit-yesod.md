---
title: Web Programming and Streaming Data in Haskell
---

# Web Programming and Streaming Data in Haskell

* Michael Snoyman
* LambdaConf 2017

---

## Overview

* How to get things done
* First hit Conduit, then hit Yesod
* Identify why you'd use these libraries
* Get you comfortable enough to use them
* More information after this talk:
    * https://haskell-lang.org/library/conduit
    * http://www.yesodweb.com/book
* Please ask questions!

---

## Prepare your machine

```
$ stack --resolver lts-8.12 --install-ghc
  build classy-prelude-yesod
```

* Your hands should be warm pretty soon
* Make sure you're plugged in or have a great battery

---

## What is streaming data?

* Process a sequence of values of the same type
* Produce a sequence of values of the same type
* Don't keep all the data in memory at once
* Perform some actions in-between
* Probably more common than you'd think

---

## Alternatives

* Lazy lists: don't allow interleaved effects
* Lazy I/O: effects, exceptions pop up in unexpected places (evil!)
* Pipes: relies on higher layers (like pipes-parse) for things built-in with Conduit
* Streaming: makes some cases (like substreams) easier, other cases (multi-consumption) more difficult

----

## Goal in this talk:

* Talk you out of using lazy I/O
* Explain when lazy lists aren't enough
* Feel free to explore other streaming libraries, but today is about Conduit

---

## Common Examples

* Read data from/write data to a file
* Communicate over a socket
* Read data from a database
* Traverse a deep directory structure
* Implement a job queue
* Generate large HTTP response bodies
* Parsing

----

## Common Non-Examples

* Highly optimized CPU pipeline
* Operations requiring no interleaved effects
* World peace

---

## Hello World: Fold

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit
main =
  print $ runConduitPure
        $ yieldMany [1..10]
       .| sumC
```

* Pure operation
* Correct: this is a bad use case for Conduit :)

----

## File Copy

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit
main = do
    -- Create a source file
    writeFile "input.txt" "This is a test."

    runConduitRes $ sourceFile "input.txt"
                 .| sinkFile "output.txt"
```

* Copies a file
* Exception safety built in (magic of `Res`)
* Common Conduit terms: source and sink

----

## Data Transform

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit
main =
  print $ runConduitPure
        $ yieldMany [1..10]
       .| mapC (+ 1)
       .| sinkList
```

* Again: you don't need Conduit for this
* Conduit most useful for pipelines

---

## Terminology

```haskell
runConduit $ foo .| bar .| baz
```

* `foo .| bar .| baz` is a *pipeline*
* `foo`, `bar`, and `baz` are *components* of the pipeline
* `foo` is *upstream* from `bar`, `baz` is *downstream* from `bar`
* `foo` can *yield* downstream to `bar`
* `baz` can *await* from `bar`/upstream
* You *run the pipeline* to perform effects/get a result

----

## Fusing

```haskell
runConduit $ foo .| bar .| baz
```

* Connect two components
* Output from upstream is the input to downstream
* Creates a new component of the two pieces fused together
* `.|` operator, or `fuse` function

----

## Streams

```haskell
runConduit $ foo .| bar .| baz
```

* `foo` sends a *stream* of values to `bar`
* The output from `foo` must match the input to `bar`
* Same thing with `bar` and `baz`
* `yield` to downstream
* `await` from upstream

----

## Results

```haskell
runConduit $ foo .| bar .| baz
```

* Single result value from a component
* When we fuse, throw away upstream result value
    * Or use `fuseUpstream` or `fuseBoth`
* Example: `sumC`
* When we run the pipeline, this is the value that comes out

----

## Pipeline

```haskell
runConduit $ foo .| bar .| baz
```

* A complete pipeline does not have any meaningful input or output
* Input: unit value `()`
* Output: `Void`
* Why the difference? Let's talk over beers...
* Quiz:
    * What's the input of `foo`?
    * What's the output of `baz`?

----

## Conduit Execution

* Start at downstream
* Keep processing until it `await`s
* Pass control to next upstream component
* If upstream `await`s, keep going up the chain
* When we `yield`, pass control back downstream
* Downstream will always get control back
* Upstream: not so much

----

## Types

```haskell
runConduit $ foo .| bar .| baz

newtype ConduitM (i :: *) (o :: *) (m :: * -> *) (r :: *)

foo :: ConduitM () a    m ()
bar :: ConduitM a  b    m ()
baz :: ConduitM b  Void m r

foo .| bar :: ConduitM () b    m ()
bar .| baz :: ConduitM a  Void m r

foo .| bar .| baz :: ConduitM () Void m r
runConduit $ foo .| bar .| baz :: m r
```

----

## Example types

__NOTE__: In all cases, requires `Monad m`

```haskell
mapC        :: (i -> o)           -> ConduitM i o m ()
foldlC      :: (r -> i -> r) -> r -> ConduitM i o m r
mapM_C      :: (i -> m ())        -> ConduitM i o m ()
repeatC     :: o                  -> ConduitM i o m ()
takeWhileC  :: (i -> Bool)        -> ConduitM i i m ()
decodeUtf8C :: MonadThrow m       => Conduit ByteString m Text
```

---

## Congratulations!

* You now know all core concepts of Conduit
* Have a good day

----

## Just Kidding

![Deeper](http://i1.kym-cdn.com/photos/images/newsfeed/000/531/557/a88.jpg)

---

## Understanding Effects

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit
loudYield :: forall i. Int -> ConduitM i Int IO ()
loudYield x = do
  liftIO $ putStrLn $ "yielding: " ++ show x
  yield x
loudSinkNull :: forall o. ConduitM Int o IO ()
loudSinkNull =
  mapM_C $ \x -> putStrLn $ "awaited: " ++ show x
main =
  runConduit $ mapM_ loudYield [1..3]
            .| loudSinkNull
```

----

## Output

```
yielding: 1
received: 1
yielding: 2
received: 2
yielding: 3
received: 3
```

Notice how control bounces back and forth between components.

----

## Explicit await

```haskell
loudSinkNull =
  loop
  where
    loop = do
      liftIO $ putStrLn "calling await"
      mx <- await
      case mx of
        Nothing -> liftIO $ putStrLn "all done!"
        Just x -> do
          liftIO $ putStrLn $ "received: " ++ show x
          loop
```

```
calling await
yielding: 1
received: 1
calling await
yielding: 2
...
calling await
all done!
```

----

## No await

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

source = liftIO $ putStrLn "Entered the source"
sink = liftIO $ putStrLn "Entered the sink"

main = runConduit $ source .| sink
```

```
Entered the sink
```

Never entered the source!

----

## Guess the output

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit
source = do
  liftIO $ putStrLn "Source 1"
  yield ()
  liftIO $ putStrLn "Source 2"

sink = do
  liftIO $ putStrLn "Sink 1"
  _ <- await
  liftIO $ putStrLn "Sink 2"

main = runConduit $ source .| sink
```

----

## Using undefined

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = runConduit $ undefined .| return ()
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = runConduit $ return () .| undefined .| return ()
```

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = runConduit $ return () .| undefined
```

---

## Finalizers

Upstream can't regain control, so...

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

source = do
  liftIO $ putStrLn "acquire some resource"
  mapM_ (\x -> yieldOr x
    (putStrLn $ "cleaning up after: " ++ show x)
    ) [1..10]

main = runConduit $ source .| takeC 2 .| printC
```

```
acquire some resource
1
2
cleaning up after: 2
```

----

## Exceptions

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

source = do
  liftIO $ putStrLn "acquire some resource"
  mapM_ (\x -> yieldOr x
    (putStrLn $ "cleaning up after: " ++ show x)
    ) [1..10]

main = runConduit $ source .| takeC 2 .| (printC >> undefined)
```

----

## ResourceT

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

source = bracketP
  (putStrLn "acquire some resource")
  (\() -> putStrLn "cleaning up")
  (\() -> mapM_ yield [1..10])

main = runConduitRes
     $ source .| takeC 2 .| (printC >> undefined)
```

----

## More on ResourceT

* Allows us to register cleanup events
* Occur even if exceptions are thrown
* Works around limitations of coroutine/CPS
* Simple cases can be replaced with bracket-pattern
* Some more complicated cases require something like `ResourceT`
    * E.g., deep directory traversal

---

## Average (bad)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = print
     $ runConduitPure
     $ yieldMany [1..10 :: Double]
    .| ((/)
            <$> sumC
            <*> (fromIntegral <$> lengthC))
```

----

## Average (good)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = print
     $ runConduitPure
     $ yieldMany [1..10 :: Double]
    .| getZipSink ((/)
            <$> ZipSink sumC
            <*> ZipSink (fromIntegral <$> lengthC))
```

Nice perk: Conduit forced us to avoid a common space leak in the list
version!

----

## Takeaways

* `Applicative` and `Monad` composition sequentially consumes upstream
* They also sequentially produce downstream
* `ZipSink` allows them to consume in parallel

---

## Folds

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = print
     $ runConduitPure
     $ yieldMany [1..10]
    .| foldlC (flip (:)) []
```

----

## Monadic folds

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = runConduit
     $ yieldMany [1..10]
    .| (foldMC f 0 >>= liftIO . print)
  where
    f total x = do
      putStrLn $ "Received: " ++ show x
      return $ total + x
```

---

## Chunked data

What's wrong with this picture?

```haskell
sinkHistogram
  :: Monad m
  => ConduitM Word8 o m (HM.HashMap Word8 Int)
sinkHistogram =
    foldlC go HM.empty
  where
    go m w = HM.insertWith (+) w 1 m
```

* Conduit does introduce an overhead
* An extra `await`/`yield` per byte is _heavy_

----

## Much better

```haskell
sinkHistogram
  :: Monad m
  => ConduitM ByteString o m (HM.HashMap Word8 Int)
sinkHistogram =
    foldlCE go HM.empty
  where
    go m w = HM.insertWith (+) w 1 m
```

* All we did was replace `foldlC` with `foldlCE`
* More generalized type signature:

```haskell
sinkHistogram
  :: (Monad m, Element i ~ Word8, MonoFoldable i)
  => ConduitM i o m (HM.HashMap Word8 Int)
```

---

## Leftovers

Guess the output

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Conduit

main = print
     $ runConduitPure
     $ yieldMany [1 .. 10 :: Int]
    .| ((,)
            <$> (takeWhileC (< 6) .| sinkList)
            <*> sinkList)
```

(Not a trick question... yet)

```
([1,2,3,4,5],[6,7,8,9,10])
```

----

## Let's implement takeWhileC

```haskell
myTakeWhileC :: Monad m
             => (i -> Bool)
             -> ConduitM i i m ()
myTakeWhileC f =
    loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x
          | f x -> yield x >> loop
          | otherwise -> return ()
```

Hmm...

```
([1,2,3,4,5],[7,8,9,10])
```

----

## Let's fix that

```haskell
myTakeWhileC :: Monad m
             => (i -> Bool)
             -> ConduitM i i m ()
myTakeWhileC f =
    loop
  where
    loop = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x
          | f x -> yield x >> loop
          | otherwise -> leftover x
```

----

## More leftovers examples

Let's step it up a notch

```haskell
main = runConduit
     $ yieldMany [1 .. 10 :: Int]
    .| do
         mapC id .| (await >>= maybe (return ()) leftover)
         printC
    .| do
         leftover "Hello There!"
         printC
```

* (Output on next slide)
* Don't forget: start downstream when processing!
* Yes, you can deeply nest Conduit components like this

----

## Output from previous slides

```
"Hello There!"
2
3
4
5
6
7
8
9
10
```

----

## Leftover lessons

* Whenever you use `leftover`, the next monadic bind picks up the
  value with `await`
* Fusion drops any leftovers (they can't be passed upstream)
    * If needed, use `fuseLeftovers`
* This is the primary reason Conduit isn't a category
* Leftovers especially useful for chunked data, e.g.
    * Read a `ByteString`
    * Consume part of the `ByteString`
    * Use `leftover` on the rest

---

## Library ecosystem

* Lots of different packages
* `conduit` provides core datatypes and basic functions
* `conduit-extra` has commonly used helpers
* `conduit-combinators`: batteries-included, chunked and unchunked

----

## My recommendation

* Use `conduit-combinators` by default
* Import `Conduit` which doesn't require qualified import
* Most names have `C` as a suffix (e.g., `foldlC`)
* Chunked versions have a `CE` suffix (for *element*, e.g., `foldlCE`)

---

# Stretch

Prepare yourselves for Yesod :)

---

## Yesod

* Web framework
* Supports traditional HTML sites and web services (usually JSON)
* Goal: turn as many common bugs into compile-time errors
* Philosophy: bring the benefits of Haskell to a standard MVC-ish framework

----

## How it works

* Built on Web Application Interface (WAI)
* Template Haskell + DSL for type-safe routing
* `Handler` monad for coding routes
* `Widget`s and templates for HTML/CSS/JS
* Many add-on libraries for common tasks (auth, forms, XML sitemaps)
* Ties in well with Persistent for type-safe database access

----

## Flexibility

* Yesod is more flexible than we'll discuss today
* Template Haskell, DSLs aren't required
* Swap out database libraries
* Host with FastCGI instead of Warp
* For those interested: http://www.yesodweb.com/book/yesod-for-haskellers

----

## "Standard" workflow

* Scaffolded site: `stack new mysite yesod-postgres`
* Built in:
    * Auth
    * Config file + env vars
    * HTML templating + Bootstrap.css
    * Logging
    * CSS minification
* Development server (`yesod devel`)

----

## What we'll cover today

* Yesod is _big_
* Focus today on mostly JSON services subset
    * Thanks to Kris Nuttycombe for this suggestion :)
* Want more? Talk to me after, or check out the book
  http://www.yesodweb.com/book

---

## Common Stuff

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings, QuasiQuotes TemplateHaskell,
             TypeFamilies, NoImplicitPrelude, ViewPatterns #-}
import ClassyPrelude.Yesod
data App = App
mkYesod "App" [parseRoutes|
...
|]
instance Yesod App
...
main = warp 3000 App
```

----

## Language extensions

* Yesod uses a bunch
* Use Persistent? That's a paddlin'
* `OverloadedStrings`? Duh
* `QuasiQuotes` and `TemplateHaskell` for routing DSL
* `TypeFamilies` are used for associated route types
* `NoImplicitPrelude` because we're using ClassyPrelude
* `ViewPatterns` is part of the generated parsing code

----

## Imports

```haskell
import ClassyPrelude.Yesod
```

* Some men just like to watch the world burn
* Also, convenient to avoid a bunch of imports in these slides

----

## Foundation data type

```haskell
data App = App
```

* Every app has a central data type
* Put config values, globals, etc, in it in your `main` function
* Access value from any `Handler` with `getYesod`
* Also used for associated route types

----

## Route definition and `mkYesod`

```haskell
mkYesod "App" [parseRoutes|
...
|]
```

* Define your routes with a DSL
* Generates a data type for your routes
* Also generates some convenience type synonyms

----

## Route example

```haskell
mkYesod "App" [parseRoutes|
/ HomeR GET
|]
```

Generates

```haskell
instance RenderRoute App where
  data Route App = HomeR
  renderRoute :: Route App -> ([Text], [(Text, Text)])
instance ParseRoute App where
  parseRoute :: ([Text], [(Text, Text)]) -> Maybe (Route App)
type Handler = HandlerT IO App
instance YesodDispatch App
```

* And a few others
* Goal: hide away tedious, error-prone boilerplate

----

## Yesod typeclass

```haskell
instance Yesod App
```

* Collection of overridable settings
* Example: how to store user session data
* Defaults are Good Enough™ in many cases
* Scaffolded site helps a lot

----

## Defining your Handlers

```haskell
mkYesod "App" [parseRoutes|
/ HomeR GET
/fibs/#Int FibsR GET
|]

getHomeR :: Handler Text
getFibsR :: Int -> Handler Value
```

* Handler names determined by convention
* Often mime-type determined by return type
* `YesodDispatch` instance uses these functions

----

## Run it!

```haskell
main :: IO ()
main = warp 3000 App
```

* `warp` is a convenient helper
    * Performs any initialization necessary (specified in `Yesod` instance)
    * Converts to a WAI `Application`
    * Runs on given port with Warp
    * Installs some standard middlewares
* `toWaiApp` or `toWaiAppPlain` == more control
* Can perform initialization before `warp` call

---

## Hello World

```haskell
mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Text
getHomeR = return "Hello World!"
```

* Only responds to `/`
* Responds with a `text/plain` mime type

----

## JSON output

```haskell
getHomeR :: Handler Value
getHomeR = return "Hello World!"
```

* Notice the difference?
* `Value` type determines `application/json`

----

## Why not both?

```haskell
getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
  provideRep $ return ("Hello World!" :: Text)
  provideRep $ return ("Hello World!" :: Value)
```

* Types determine mime per representation
* No accept header: use first
* Otherwise, fiinds match
* No match: returns a `406 Not Acceptable`

----

## Arbitrary mime types

```haskell
getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
  provideRep $ return ("Hello World!" :: Text)
  provideRep $ return ("Hello World!" :: Value)
  provideRepType "text/csv" $ return ("hello,world\n" :: Text)
```

---

## Route parameters

```haskell
mkYesod "App" [parseRoutes|
/          HomeR GET
/fibs/#Int FibsR GET
|]

getHomeR :: Handler ()
getHomeR = redirect $ FibsR 1

getFibsR :: Int -> Handler Value
getFibsR i = do
  render <- getUrlRender
  return $ object
    [ "value" .= (fibs !! i)
    , "next"  .= render (FibsR (i + 1)) ]
```

* Values is parsed and passed into the handler
* Route type makes data cons with arguments

----

## Query string parameters

```haskell
mkYesod "App" [parseRoutes|
/ FibsR GET
|]

getFibsR :: Handler Value
getFibsR = do
  mi <- lookupGetParam "index"
  let i = fromMaybe 1 $ mi >>= readMay . unpack
  render <- getUrlRenderParams
  return $ object
    [ "value" .= (fibs !! i)
    , "next" .= render FibsR [("index", tshow (i + 1))]
    ]
```

* We love fibs :)
* Lookup parameters easily (also: forms support)
* Render URLs with and without parameter lists

----

## POST parameters

```haskell
mkYesod "App" [parseRoutes|
/ FibsR PUT
|]

putFibsR :: Handler Value
putFibsR = do
  mi <- lookupPostParam "index"
  let i = fromMaybe 1 $ mi >>= readMay . unpack
  return $ object
    [ "value" .= (fibs !! i)
    ]
```

    curl -i http://localhost:3000/ -X PUT -d index=4

* `PUT` method, but still call them POST params :(
* Again, form support is available

----

## POST files

```haskell
mkYesod "App" [parseRoutes|
/ HomeR PUT
|]

putHomeR :: Handler Value
putHomeR = do
  Just fileInfo <- lookupFile "some-file"
  size <- runConduitRes $ fileSource fileInfo .| lengthCE
  return $ object
    [ "name" .= fileName fileInfo
    , "content-type" .= fileContentType fileInfo
    , "size" .= (size :: Int)
    ]
```

    curl -i http://localhost:3000/ -X PUT -F some-file=@image.png

* Yay conduit!
* Want all POST info? `runRequestBody`

----

## JSON request body

```haskell
putHomeR :: Handler Value
putHomeR = requireCheckJsonBody
```

    curl -i http://localhost:3000/ -X PUT \
        -H "Content-Type:application/json" \
        -d '{"foo":"bar"}'

* Dumb echo server
* Uses any `FromJSON` instance
* `Check` says "check mime-type before parsing"
    * Backwards compat can be annoying :)

---

## Header echo

```haskell
getHomeR :: Handler ()
getHomeR = do
  mvalue <- lookupHeader "marco"
  forM_ mvalue $ addHeader "polo" . decodeUtf8
```

    curl -i http://localhost:3000/ -H "Marco:Hello"

* Case-insensitive lookup
* Text vs ByteString: yes, it's annoying
* Oh, yes, you can just return unit

---

## Permissions

```haskell
getHomeR :: Handler Text
getHomeR = do
  mpassword <- lookupGetParam "password"
  case mpassword of
    Just "12345" -> return "Hello President Skroob"
    _ -> permissionDenied "Self Destruct Initiated"
```

* Don't actually use GET params for passwords

----

## Route Attributes and `isAuthorized`

```haskell
mkYesod "App" [parseRoutes|
/ HomeR GET !admin
|]
instance Yesod App where
  isAuthorized route _isWrite
    | "admin" `member` routeAttrs route = do
        mpassword <- lookupGetParam "password"
        case mpassword of
          Just "12345" -> return Authorized
          _ -> return $ Unauthorized "Self Destruct Initiated"
    | otherwise = return Authorized
getHomeR :: Handler Text
getHomeR = return "Hello President Skroob"
```

Separate those concerns!

----

## Session values

```haskell
mkYesod "App" [parseRoutes|
/     HomeR GET  !admin
/auth AuthR POST
|]

getHomeR :: Handler Text
getHomeR = return "Hello President Skroob"

postAuthR :: Handler ()
postAuthR = do
  mpassword <- lookupPostParam "password"
  case mpassword of
    Just "12345" -> setSession "AUTH" "Yes"
    _ -> permissionDenied "Self Destruct Initiated"
```

* Sets a key to a value in the user session
* Default: HMAC-secured client session key in a cookie
* Code continues...

----

## Session based auth functions

```haskell
instance Yesod App where
  authRoute _ = Just AuthR
  isAuthorized route _isWrite
    | "admin" `member` routeAttrs route = do
        mauth <- lookupSession "AUTH"
        case mauth of
          Just "Yes" -> return Authorized
          _ -> return AuthenticationRequired
    | otherwise = return Authorized
```

* `authRoute` is where users are redirected
* In a full app: `GET AuthR` would give a user-friendly page

----

## Real world auth

* yesod-auth provides lots of backends
* OpenID, Google Email, local email...
* I personally really like third party auth
* Still sad that Mozilla Persona shut down

---

## Streaming request body

```haskell
import Text.XML.Stream.Parse

mkYesod "App" [parseRoutes|
/ HomeR PUT
|]

putHomeR :: Handler Value
putHomeR = do
  events <- runConduit $ rawRequestBody .| parseBytes def .| lengthC
  return $ object ["event-count" .= (events :: Int)]
```

* Conduit to the rescue
* Request body = stream of `ByteString`
* Request body can be consumed once!

----

## Streaming response body

```haskell
import Data.ByteString.Builder (intDec)

getHomeR :: Handler TypedContent
getHomeR = respondSource "text/csv" $ do
  yield $ Chunk "number,plus1\n"
  forM_ [1..100 :: Int] $ \i -> yield
    $ Chunk $ intDec i <> "," <> intDec (i + 1) <> "\n"
```

* Again with the conduits
* Use the `data Flush a = Flush | Chunk a` type
* ByteString Builders under the surface
* A little tedious, so...

----

## Convenient streaming functions

```haskell
getHomeR :: Handler TypedContent
getHomeR = respondSource "text/csv" $ do
  sendChunkText "number,plus1\n"
  forM_ [1..100 :: Int] $ \i -> sendChunkText $ mconcat
      [ tshow i
      , ","
      , tshow (i + 1)
      , "\n"
      ]
```

* Avoid need to use explicit `Chunk` constructor
* Use `Text` or `ByteString` instead of `Builder`

---

## Config files (1)

```yaml
aws-secret: _env:AWS_SECRET
home-response: _env:HOME_RESPONSE:Hello World
```

```haskell
data Config = Config
  { awsSecret :: !Text
  , homeResponse :: !Text
  }
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config
    <$> o .: "aws-secret"
    <*> o .: "home-response"
```

* Special syntax in YAML to allow env overriding
* aws-secret: must have an env var
* home-response: optional
* `FromJSON`: normal aeson code

----

## Config files (2)

```haskell
data App = App !Config

getHomeR :: Handler Text
getHomeR = do
  App config <- getYesod
  return $ homeResponse config

main :: IO ()
main = do
  config <- loadYamlSettingsArgs [] useEnv
  warp 3000 $ App config
```

* Stick `Config` inside `App`
* Get `Config` with `getYesod`
* Initialize `Config` with `loadYamlSettingsArgs`

----

## Config files (3)

```
$ ./Main.hs
Main.hs: loadYamlSettings: No configuration provided
$ ./Main.hs config.yaml
Main.hs: Could not convert to AppSettings: expected Text,
         encountered Null
$ AWS_SECRET=foobar ./Main.hs config.yaml
Application launched ^C
$ AWS_SECRET=foobar HOME_RESPONSE=Goodbye \
  ./Main.hs config.yaml
Application launched ^C
```

* Must provide config file(s) on command line
* Must provide `AWS_SECRET`
* If provided, `HOME_RESPONSE` changes that response payload

---

## Learn More

* http://www.yesodweb.com/
* https://github.com/yesodweb/yesod-cookbook
* Example code bases
    * https://github.com/snoyberg/haskellers
    * https://github.com/yesodweb/yesodweb.com
