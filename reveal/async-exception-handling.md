---
title: Async exception handling in Haskell
---

## Async exception handling in Haskell

* Michael Snoyman
* VP of Engineering
* FP Complete webinar
* April 11, 2018

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
</div>

---

## From the beginning

* Many languages have synchronous exceptions
* Double-edged sword
    * Arguably easier to write correct code
    * Can lead to lack of resource cleanup
* GHC Haskell has asynchronous exceptions too
    * Let's just say "Haskell," you know what I mean now
* Gotta cover exceptions to get to async exceptions

----

## What we'll cover today

* Defining different types of exceptions
* Correct synchronous exception handling
* How bottom values play in
* Basics of async exceptions
* Masking and uninterruptible masking
* Helper libraries
* Some more complex examples

Lots of ground to cover before we talk about async stuff!

----

## Fear is the mind-killer

* Async exceptions _are_ tricky
* They aren't nearly as terrifying as lore makes them out as
* Usually: use the right helper library, everything's good

----

## Are exceptions good or bad?

* Not our topic today!
* Lots of healthy debate inside and outside the Haskell community
* However: runtime exceptions are the reality of GHC Haskell today
* Whether you like it or not: need to deal with it

----

## Teaser

Goal for this talk: you should see multiple reasons I call this
function `badRace`:

```haskell
badRace :: IO a -> IO b -> IO (Either a b)
badRace ioa iob = do
  mvar <- newEmptyMVar
  tida <- forkIO $ ioa >>= putMVar mvar . Left
  tidb <- forkIO $ iob >>= putMVar mvar . Right
  res <- takeMVar mvar
  killThread tida
  killThread tidb
  return res
```

---

## Motivating example

* Most complexity around scarce resource handling
* File handling great example, we'll use it
    * Open the file, may fail
    * Interact with the file handle, may fail
    * Close the file handle regardless
    * File descriptors are scarce!
* Start without any exceptions, build up from there
* Slight detour though...

----

## Pure code

* Cannot catch exceptions in pure code
* Makes sense: no resource allocation in pure code
* Except...
* Can throw from pure code &#x1f641;
* Can use `unsafePerformIO` for allocations
* Memory can be allocated implicitly
    * Not a contradiction! Memory ain't scarce
* Technically can use `unsafePerformIO` to catch

Overall: our focus is on non-pure, `IO` code. Slight reference to
transformers later.

----

## The land of no exceptions

Haskell without any runtime exceptions (great rejoicing in the land)

```haskell
openFile :: FilePath -> IOMode
         -> IO (Either IOException Handle)
hClose :: Handle -> IO () -- assume it can never fail
usesFileHandle :: Handle -> IO (Either IOException MyResult)

myFunc :: FilePath -> IO (Either IOException MyResult)
myFunc fp = do
  ehandle <- openFile fp ReadMode
  case ehandle of
    Left e -> return (Left e)
    Right handle -> do
      eres <- usesFileHandle handle
      hClose handle
      return eres
```

----

## Land of synchronous exceptions

Add two new primitives for synchronous exceptions

```haskell
throwIO :: IOException -> IO a
try :: IO a -> IO (Either IOException a)
```

__Synchronous exceptions are exceptions which are generated directly
from the `IO` actions you are calling.__

----

## Rewrite our function

```haskell
openFile :: FilePath -> IOMode -> IO Handle
hClose :: Handle -> IO ()
usesFileHandle :: Handle -> IO MyResult

myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  res <- usesFileHandle handle
  hClose handle
  return res
```

* Code is shorter
* Can't tell whether `openFile` and `hClose` can fail
* No need to pattern match on `openFile` result
* But wait! What if `usesFileHandle` throws an exception?

----

## Try and throw

Fix it!

```haskell
myFunc :: FilePath -> IO MyResult
myFunc fp = do
  handle <- openFile fp ReadMode
  eres <- try (usesFileHandle handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

(Synchronous) exception safe!

----

## Capture the pattern

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res

myFunc :: FilePath -> IO MyResult
myFunc fp = withFile fp ReadMode usesFileHandle
```

__General principle__: Avoid using functions which only allocate or
only clean up whenever possible.

__Question__: What if `cleanup` throws an exception?

---

## Extensible exceptions

* We assumed `IOException` above
* GHC has OO-style extensibility, like Java
    * Please don't vomit

```haskell
data SomeException = forall e . Exception e => SomeException e

class (Typeable e, Show e) => Exception e where
  toException   :: e -> SomeException
  fromException :: SomeException -> Maybe e

throwIO :: Exception e => e -> IO a
try :: Exception e => IO a -> IO (Either e a)
```

----

## Example exception, no hierarchy

```haskell
data InvalidInput = InvalidInput String
  deriving (Show, Typeable)
instance Exception InvalidInput where
  toException ii = SomeException ii
  fromException (SomeException e) = cast e -- part of Typeable
```

`toException` and `fromException` have defaults, so...

```haskell
instance Exception InvalidInput
```

----

## Example of hierarchy

```haskell
data MyAppException
  = InvalidInput String
  | SomethingElse SomeException
  deriving (Show, Typeable)
instance Exception MyAppException
```

```haskell
data SubException = NetworkFailure String
  deriving (Show, Typeable)
instance Exception SubException where
  toException = toException . SomethingElse . SomeException
  fromException se = do
    SomethingElse (SomeException e) <- fromException se
    cast e
```

```haskell
main :: IO ()
main = do
  e <- try $ throwIO $ NetworkFailure "Hello there"
  print (e :: Either SomeException ())
```

---

## Exception in pure code

* Why call it `throwIO` and not `throw`?

```haskell
throw :: Exception e => e -> a
```

* Not an async exception!
* I call them __impure exceptions__
* Create bottom values

----

## Creating impure exceptions

* Using the `throw` function directly
* Using a function which calls `throw`, like `error`
* Using partial functions like `head`
* Incomplete pattern matches (GHC automatically inserts the equivalent
  of a call to `throw`)
* Creating infinite loops in pure code, where GHC's runtime _may_
  detect the infinite loop and throw a runtime exception

----

## Preaching to the choir

* Partiality is bad, m'kay?
* Avoid creating these impure exceptions

----

## Challenge: what's the output?

```haskell
import Control.Exception
import Data.Typeable

data Dummy = Dummy
  deriving (Show, Typeable)
instance Exception Dummy

printer :: IO (Either Dummy ()) -> IO ()
printer x = x >>= print
```

```haskell
main :: IO ()
main = do
  printer $ try $ throwIO Dummy
  printer $ try $ throw Dummy
  printer $ try $ evaluate $ throw Dummy
  printer $ try $ return $! throw Dummy
  printer $ try $ return $ throw Dummy
```

----

## Case 1

```
printer $ try $ throwIO Dummy
Left Dummy
```

We're using proper runtime exceptions via `throwIO`, and therefore
`Dummy` is thrown immediately as a runtime exception. Then `try` is
able to catch it, and all works out well.

----

## Case 2

```
printer $ try $ throw Dummy
Left Dummy
```

We generate a value of type `IO ()` which, when evaluated, will throw
a `Dummy` value. Passing this value to `try` forces it immediately,
causing the runtime exception to be thrown. The result ends up being
identical to using `throwIO`.

----

## Case 3

```
printer $ try $ evaluate $ throw Dummy
Left Dummy
```

`throw Dummy` has type `()`. The `evaluate` function then forces
evaluation of that value, which causes the `Dummy` exception to be
thrown.

----

## Case 4

```
printer $ try $ return $! throw Dummy
Left Dummy
```

This is almost identical; it uses `$!`, which under the surface uses
`seq`, to force evaluation. We're not going to dive into the
difference between `evaluate` and `seq` today.

----

## Case 5

```
printer $ try $ return $ throw Dummy
Right Main.hs: Dummy
```

* Odd man out
* Create thunk with `throw Dummy` of type `()`
* `return` wraps it into `IO ()`
* `try` forces evaluation of `IO ()`, which doesn't force evaluation of the `()`
* End up with value of type `Either Dummy ()`
* Equivalent to `Right (throw Dummy)`
* `printer` tries to print it, forces `throw Dummy`, causes crash

----

## What's the upshot?

* Not passing judgement, but: don't use `throw` and `error`
    * If you use exceptions, use `throwIO`
* Pure exceptions seem to appear at "random"
* But the trigger for it getting thrown is always local
    * Forcing evaluation inside `IO`
* Therefore, by our definition, impure exceptions _are_ synchronous
  exceptions
* We'll treat them as such, but mostly just ignore them, because...

----

## Impure exceptions are irrelevant

Who cares if `inner` returns a partial/bottom value?

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  handle <- openFile fp mode
  eres <- try (inner handle)
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

We never evaluate it in `withFile`, so it doesn't break anything

---

## Motivating async exceptions

We want a `timeout` function

```haskell
timeout :: Int -- microseconds
        -> IO a -> IO (Maybe a)
```

Can we get something like this without async exceptions?

----

## Bad approach: built in primitive

Imagine: part of the runtime system, kills thread immediately

```haskell
timeout 1000000 $ bracket
  (openFile "foo.txt" ReadMode)
  hClose
  somethingReallySlow
```

* `hClose` will never get called
* Defeats exception safety

----

## Outside the runtime (1)

```haskell
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when, forever)
import Data.IORef
import Data.Typeable

data Timeout = Timeout
  deriving (Show, Typeable)
instance Exception Timeout

type CheckTimeout = IO ()
```

----

## Outside the runtime (2)

```haskell
timeout :: Int -> (CheckTimeout -> IO a) -> IO (Maybe a)
timeout micros inner = do
  retval <- newEmptyMVar
  expired <- newIORef False
  let checkTimeout = do
        expired' <- readIORef expired
        when expired' $ throwIO Timeout
  _ <- forkIO $ do
    threadDelay micros
    writeIORef expired True
```

```haskell
  _ <- forkIO $ do
    eres <- try $ inner checkTimeout
    putMVar retval $
      case eres of
        Left Timeout -> Nothing
        Right a -> Just a
  takeMVar retval
```

----

## Outside the runtime (3)

```haskell
myInner :: CheckTimeout -> IO ()
myInner checkTimeout = bracket_
  (putStrLn "allocate")
  (putStrLn "cleanup")
  (forever $ do
    putStrLn "In myInner"
    checkTimeout
    threadDelay 100000)

main :: IO ()
main = timeout 1000000 myInner >>= print
```

----

## Outside the runtime (4)

Positive: reuses existing exception machinery, so it's
safe. Negatives:

* Cannot interrupt pure code (`checkTimeout` is in `IO`)
* Have to remember to call `checkTimeout`, or `timeout` will break

__BONUS__ The code above has a potential deadlock in it due to mishandling of
synchronous exceptions. Try and find it!

---

## Real asynchronous exceptions

__Async exceptions are exceptions thrown from another thread.__

* Local thread does not cause the exception
* Bubble up just like synchronous exceptions
* Caught with `try` (and friends like `catch`)
* Difference is how they're thrown

```haskell
forkIO :: IO () -> IO ThreadId
throwTo :: Exception e => ThreadId -> e -> IO ()
```

----

## Compare to hand-written `timeout`

* `throwTo` like setting `expired` to `True`
* Runtime automatically calls `checkTimeout` equivalent
* Runtime __can detect async exception at any point__
* Can happen at unexpected times, leading to new problems

----

## The need for masking

Let's revisit our `withFile`, with explicit async-exception checking
calls

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = do
  checkAsync -- 1
  handle <- openFile fp mode
  checkAsync -- 2
  eres <- try (inner handle)
  checkAsync -- 3
  hClose handle
  checkAsync -- 4
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

1 or 4: fine, 2 or 3: bad news bears

----

## The `mask_` function

Let's temporarily block async exceptions

```haskell
mask_ :: IO a -> IO a

withFile fp mode inner = mask_ $ ...
```

* Fixes the resource leak
* But now `timeout` can't kill `inner`!
* Need to restore the previous masking state

----

## The `mask` function

Mask, but get a function to restore

```haskell
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
```

__ADVANCED__ This restores instead of unmasking to deal with nested
maskings. This deals with the "wormhole" problem, which we won't
cover.

----

## Restore


```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile fp mode inner = mask $ \restore -> do
  handle <- openFile fp mode
  eres <- try (restore (inner handle))
  hClose handle
  case eres of
    Left e -> throwIO e
    Right res -> return res
```

* Safe to `restore`, because it's wrapped in a `try`
* No way for any exceptions to prevent `hClose`

---

## Catch 'em all!

* Code doesn't type check
* Which instance of `Exception`?
* Catch all exceptions with one weird trick

Replace

```haskell
eres <- try (restore (inner handle))
case eres of
  Left e -> throwIO e
```

with

```haskell
eres <- try (restore (inner handle))
case eres of
  Left e -> throwIO (e :: SomeException)
```

----

## Catch 'em all again

Code above is good, but what about this?

```haskell
main :: IO ()
main = do
  start <- getCurrentTime
  res <- timeout 1000000 $ do
    x <- try $ threadDelay 2000000
    threadDelay 2000000
    return x
  end <- getCurrentTime
  putStrLn $ "Duration: " ++ show (diffUTCTime end start)
  putStrLn $ "Res: " ++
             show (res :: Maybe (Either SomeException ()))
```

----

## The problem

Output

```
Duration: 3.004385s
Res: Just (Left <<timeout>>)
```

* Duration is 3 seconds, not 1 second
* We get a `Just` instead of a `Nothing`
* Inside the `Just` is an exception from the timeout

We caught an async exception. Why is this bad here?

----

## Recover versus cleanup

__You cannot recover from an asynchronous exception__

Two reasons to catch an exception

* __Cleanup__: you catch, perform the cleanup action, and
  rethrow. OK for async exception.
* __Recover__: catch the exception and continue with something else
  without rethrowing. Not OK for async exception.

Recovering from async exception will break functions like `timeout`

---

## GHC's async exception flaw

* Difference between sync and async: `throwIO` vs `throwTo`
* When catching: no way to see which function threw it!
* Two different techniques to approximate
    * Older technique: fork a thread
    * Newer technique: rely on types
* We'll use the latter
* __WARNING__ It's possible to spoof this!

----

## SomeAsyncException

"Superclass" of all asynchronous exceptions

```haskell
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
```

Now to fix `throwIO` and `throwTo`. But first...

----

## Wrapper types

```haskell
data SyncExceptionWrapper =
  forall e. Exception e => SyncExceptionWrapper e
instance Exception SyncExceptionWrapper

data AsyncExceptionWrapper =
  forall e. Exception e => AsyncExceptionWrapper e
instance Exception AsyncExceptionWrapper where
    toException = toException . SomeAsyncException
    fromException se = do
        SomeAsyncException e <- fromException se
        cast e
```

----

## Converters

```haskell
toSyncException :: Exception e => e -> SomeException
toSyncException e =
    case fromException se of
        Just (SomeAsyncException _) ->
          toException (SyncExceptionWrapper e)
        Nothing -> se
  where
    se = toException e
```

```haskell
toAsyncException :: Exception e => e -> SomeException
toAsyncException e =
    case fromException se of
        Just (SomeAsyncException _) -> se
        Nothing -> toException (AsyncExceptionWrapper e)
  where
    se = toException e
```

----

## Replacement throwers

```haskell
import qualified Control.Exception as EUnsafe

throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . EUnsafe.throwIO . toSyncException

throwTo :: (Exception e, MonadIO m) => ThreadId -> e -> m ()
throwTo tid = liftIO . EUnsafe.throwTo tid . toAsyncException

impureThrow :: Exception e => e -> a
impureThrow = EUnsafe.throw . toSyncException
```

----

## Replacement catchers

Break up all "catching" functions into recovery and cleanup, e.g.:

* Recovery (rethrows immediately on async)
  * `catch`
  * `try`
  * `handle`
* Cleanup (always rethrows after running cleanup)
  * `bracket`
  * `onException`
  * `finally`

----

## Simplified catch

```haskell
import qualified Control.Exception as EUnsafe

catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch f g = f `EUnsafe.catch` \e ->
  if isSyncException e
    then g e
    -- intentionally rethrowing an async exception
    -- synchronously, since we want to preserve
    -- async behavior
    else EUnsafe.throwIO e
```

Real version has some monad transformer nonsense involved

----

## Easy, safe async handling

* Stick to these modified helper functions
* You won't accidentally recover from an async exception
* Can even safely do Pokemon exception handling

```haskell
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny = try

main :: IO ()
main = tryAny (readFile "foo.txt") >>= print
```

We'll talk about libraries a bit later

---

## Avoid low level masking

* Low level masking is complicated
* Avoid it whenever possible
* Use higher level helper functions from helper modules
* But, sometimes, you'll need to know about this
* So we need to cover one more complication

----

## Uninterruptible masking

* `mask` prevents async exception polling happening _everywhere_
* However, it still allows "interruptible" actions to receive async exceptions
* Useful for avoiding deadlocks

```haskell
mask $ \restore -> do
  a <- takeMVar m
  restore (...) `catch` \e -> ...
```

* But sometimes this can _also_ prevent cleanup from running

----

## This is complicated

* Tradeoff between deadlock and guaranteed cleanup is tricky
* Github issue: https://github.com/fpco/safe-exceptions/issues/3
* Nifty trick: you can always "upgrade" `mask` to `uninterruptibleMask`
    * Doesn't work from unmasked to masked
* If you have to use a masking function, think hard about which one you want
    * Better yet: use an existing helper function

----

## Deadlock detection

What's the result of running this program?

```haskell
import Control.Concurrent

main :: IO ()
main = do
  mvar <- newEmptyMVar
  takeMVar mvar
```

Usually, it will be:

```
foo.hs: thread blocked indefinitely in an MVar operation
```

GHC runtime tries to detect `MVar` and `STM`-based deadlocks, but
won't always succeed (don't rely on it!)

----

## Uninterruptible deadlock

How about this?

```haskell
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  mvar <- newEmptyMVar
  uninterruptibleMask_ $ takeMVar mvar
```

Actual deadlock occurs: async exception is blocked

----

## Interruptible deadlock

And normal mask?

```haskell
import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
  mvar <- newEmptyMVar
  mask_ $ takeMVar mvar
```

* Async exception is received, since `takeMVar` is interruptible
* Good example of (un)interruptible
* And deadlock detection is handled via async exceptions... Right?

----

## Is it async?

How about this one?

```haskell
import Control.Concurrent
import UnliftIO.Exception

main :: IO ()
main = do
  mvar <- newEmptyMVar :: IO (MVar ())
  tryAny (takeMVar mvar) >>= print
  putStrLn "Looks like I recovered!"
```

* `tryAny` catches all synchronous exceptions (based on type)
* Guess: will "Looks like I recovered!" be printed?

----

## Recovery allowed!

* `BlockedIndefinitely` exceptions are considered synchronous
* They're technically thrown from another thread
* However, they're generated by a local action
* Totally safe to recover from
* Confusing? Yes! Logical? Arguably yes!

---

## Helper library breakdown

Three libraries provide async exception safe APIs

* `enclosed-exceptions`: based on the older forking-based approach
  (not covered here). I no longer recommend it
* `safe-exceptions`: types + the `exceptions` package
* `unliftio`: types + the `MonadUnliftIO` typeclass (my recommendation)

See also: https://www.youtube.com/watch?v=KZIN9f9rI34

---

## Rules for async safe handling

* If something _must_ happen, you must use a masking function
* If you catch an async exception, you must rethrow it ASAP
    * No long cleanup code!
* You should minimize the amount of time spent in masked state

Using the right libraries will make this much simpler!

__ONTO THE EXAMPLES__

---

## Control flow

Who likes this code?

```haskell
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad

main :: IO ()
main = do
  messages <- newChan
  race_
    (mapM_ (writeChan messages) [1..10 :: Int])
    (forever $ do
      readChan messages >>= print
      -- simulate some I/O latency
      threadDelay 100000)
```

Drops messages on the floor!

----

## Avoid async exceptions if you can

```haskell
import UnliftIO (concurrently_, atomically, finally)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TBMQueue
import Data.Function (fix)
```

```haskell
main = do
  messages <- newTBMQueueIO 5
  concurrently_
    (mapM_ (atomically . writeTBMQueue messages)
           [1..10 :: Int]
     `finally` atomically (closeTBMQueue messages))
    (fix $ \loop -> do
      mmsg <- atomically $ readTBMQueue messages
      case mmsg of
        Nothing -> return ()
        Just msg -> do
          print msg
          -- simulate some I/O latency
          threadDelay 100000 >> loop)
```

---

## Email challenge 1

Good or bad?

```haskell
bracket
  openConnection closeConnection $ \conn ->
    bracket
      (sendHello conn)
      (sendGoodbye conn)
      (startConversation conn)
```

----

## Solution: bad!

* `bracket` for open and close: good
* `bracket` for sending goodbye: takes too long
* Network protocol demands it? Network protocol is broken!
    * SIGKILL
    * Machine dies
    * Network disconnects

----

## Better code

```haskell
bracket
  openConnection closeConnection $ \conn -> do
    sendHello conn
    res <- startConversation conn
    sendGoodbye conn
    return res
```

There are likely exceptions to this rule (no pun intended), but you
should justify each such exception very strongly.

---

## Email challenge 2

Good or bad `bracket`?

```haskell
bracket before after inner = mask $ \restore -> do
  resource <- before
  eresult <- try $ restore $ inner resource
  after resource
  case eresult of
    Left e -> throwIO (e :: SomeException)
    Right result -> return result
```

(Obviously, __don't write your own bracket__!)

----

## Solution: mostly good

* Masks exceptions around entire block: good!
* `before` is run with exceptions still masked: good!. `restore` would
  break it
* `restore` inside of `try` around `inner`: good!
* Call `after` immediately after `inner`: good!
* Possible problem: should `after` have an `uninterruptibleMask`? [Arguably](https://github.com/fpco/safe-exceptions/issues/3)
* Rethrow the exception: good!

---

## Racing reads

What is the output of this program?

```haskell
import Control.Concurrent
import Control.Concurrent.Async

main :: IO ()
main = do
  chan <- newChan
  mapM_ (writeChan chan) [1..10 :: Int]
  race (readChan chan) (readChan chan) >>= print
  race (readChan chan) (readChan chan) >>= print
  race (readChan chan) (readChan chan) >>= print
  race (readChan chan) (readChan chan) >>= print
  race (readChan chan) (readChan chan) >>= print
```

----

## Solution (on my machine)

```
Left 1
Left 3
Left 5
Left 7
Left 9
```

* Non-deterministic
* Left or Right could finish first
* Maybe the second read never started
* Could even get the even numbers

Maybe that seemd far-fetched...

----

## Timed read

This is logical. Is it valid?

```haskell
timeout 1000000 $ readChan chan
```

What if we simulate weird thread scheduling?

```haskell
mapM_ (writeChan chan) [1..10 :: Int]
mx <- timeout 1000000 $ do
  x <- readChan chan
  threadDelay 2000000
  return x
print mx
readChan chan >>= print
```

----

## How to timeout?

Get inventive!

```haskell
tchan <- newTChanIO
atomically $ mapM_ (writeTChan tchan) [1..10 :: Int]
delayDone <- registerDelay 1000000
let stm1 = do
      isDone <- readTVar delayDone
      check isDone
      return Nothing
    stm2 = do
      x <- readTChan tchan
      unsafeIOToSTM $ threadDelay 2000000
      return $ Just x
mx <- atomically $ stm1 <|> stm2
print mx
atomically (readTChan tchan) >>= print
```

---

## Forked threads

__Caveat__

* Use the `async` library wherever possible
* Prefer `concurrently`, `race`, etc
* Then use `Async`
* Only then use `forkIO` as a last resort

OK, that's out of the way...

----

## Cleanup in child

```haskell
main = do
  putStrLn "Acquire in main thread"
  tid <- forkIO $
    (putStrLn "use in child thread" >> threadDelay maxBound)
      `finally` putStrLn "cleanup in child thread"
  killThread tid -- built on top of throwTo
  putStrLn "Exiting the program"
```

Timing-dependent output:

```
Acquire in main thread
Exiting the program
```

Child doesn't call `finally` before it's killed!

----

## Unhelpful masking

```haskell
main = do
  putStrLn "Acquire in main thread"
  tid <- forkIO $ uninterruptibleMask_ $
    (putStrLn "use in child thread" >> threadDelay maxBound)
      `finally` putStrLn "cleanup in child thread"
  killThread tid -- built on top of throwTo
  putStrLn "Exiting the program"
```

Still didn't call `uninterruptibleMask_` before thread is killed!

----

## Mask before forking

Masking state is inherited, so:

```haskell
main = do
  putStrLn "Acquire in main thread"
  tid <- uninterruptibleMask_ $ forkIO $
    (putStrLn "use in child thread" >> threadDelay maxBound)
      `finally` putStrLn "cleanup in child thread"
  killThread tid -- built on top of throwTo
  putStrLn "Exiting the program"
```

What's the problem?

<div class="fragment">Deadlock! Cannot kill the child</div>

----

## Almost correct

Mask and restore

```haskell
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Acquire in main thread"
  tid <- uninterruptibleMask $ \restore -> forkIO $
    restore (putStrLn "use in child thread"
             >> threadDelay maxBound)
      `finally` putStrLn "cleanup in child thread"
  killThread tid -- built on top of throwTo
  putStrLn "Exiting the program"
```

The problem is "nuanced"

----

## The best solution

Use `forkIOWithUnmask`

```haskell
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Acquire in main thread"
  tid <- uninterruptibleMask_ $ forkIOUnmask $ \unmask ->
    unmask (putStrLn "use in child thread"
            >> threadDelay maxBound)
      `finally` putStrLn "cleanup in child thread"
  killThread tid -- built on top of throwTo
  putStrLn "Exiting the program"
```

Let's see why

----

## Inherited masking state (1)

```haskell
foo = mask $ \restore -> restore getMaskingState >>= print

bar = mask $ \restore -> do
  forkIO $ restore getMaskingState >>= print
  threadDelay 10000

baz = mask_ $ do
  forkIOWithUnmask $ \unmask ->
    unmask getMaskingState >>= print
  threadDelay 10000
```

----

## Inherited masking state (2)

```haskell
main = do
  putStrLn "foo"
  foo
  mask_ foo
  uninterruptibleMask_ foo
  putStrLn "\nbar"
  bar
  mask_ bar
  uninterruptibleMask_ bar
  putStrLn "\nbaz"
  baz
  mask_ baz
  uninterruptibleMask_ baz
```

----

## Inherited masking state (3)

```
foo
Unmasked
MaskedInterruptible
MaskedUninterruptible

bar
Unmasked
MaskedInterruptible
MaskedUninterruptible

baz
Unmasked
Unmasked
Unmasked
```

In forked thread: want to be unmasked, _not_ the previous masking
state!

---

## forkIO and race

Remember me?

```haskell
badRace :: IO a -> IO b -> IO (Either a b)
badRace ioa iob = do
  mvar <- newEmptyMVar
  tida <- forkIO $ ioa >>= putMVar mvar . Left
  tidb <- forkIO $ iob >>= putMVar mvar . Right
  res <- takeMVar mvar
  killThread tida
  killThread tidb
  return res
```

```haskell
main :: IO ()
main = badRace (return ()) (threadDelay maxBound) >>= print
```

```
Left ()
```

Good

----

## Masked?

```haskell
main :: IO ()
main = mask_
     $ badRace (return ()) (threadDelay maxBound)
   >>= print
```

Same thing: `Left ()`. But what about:

```haskell
main :: IO ()
main = uninterruptibleMask_
     $ badRace (return ()) (threadDelay maxBound)
   >>= print
```

Deadlock! `forkIO` inside `badRace` inherits masked state

----

## Fixing this bug

(There are other bugs, exercise for the reader)

```haskell
badRace :: IO a -> IO b -> IO (Either a b)
badRace ioa iob = do
  mvar <- newEmptyMVar
  tida <- forkIOWithUnmask $ \u -> u ioa
    >>= putMVar mvar . Left
  tidb <- forkIOWithUnmask $ \u -> u iob
    >>= putMVar mvar . Right
  res <- takeMVar mvar
  killThread tida
  killThread tidb
  return res
```

---

## unsafePerformIO vs unsafeDupablePerformIO

* Couldn't get a good demonstration
* Great example of what goes wrong without async exceptions
* If you're curious, Trac ticket: https://ghc.haskell.org/trac/ghc/ticket/8502
    * c/o Chris Allen, thanks!

---

## Links (1)

* General Haskell syllabus: https://www.fpcomplete.com/haskell-syllabus
* The `unliftio` library: https://www.stackage.org/package/unliftio
    * Exception handling module: https://www.stackage.org/haddock/lts-11.1/unliftio-0.2.5.0/UnliftIO-Exception.html
* safe-exceptions documentation: https://haskell-lang.org/library/safe-exceptions

----

## Links (2)

* Exceptions best practices: https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
* Monad transformers talk
    * Slides: https://www.snoyman.com/reveal/monad-transformer-state
    * Video: https://www.youtube.com/watch?v=KZIN9f9rI34

---

## Questions? Comments?

* Thanks all!
* Let us know on Twitter what topics you're interested in next
    * [@snoyberg](https://twitter.com/snoyberg/)
    * [@FPComplete](https://twitter.com/fpcomplete)
* For Partnering opportunities please email: [Robert@fpcomplete.com](mailto:Robert@fpcomplete.com)
* Slides: https://www.snoyman.com/reveal/async-exception-handling
* Blog post on its way!
