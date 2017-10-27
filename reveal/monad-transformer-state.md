---
title: Everything you didn't want to know about monad transformer state
---

### Monad Transformer State

*Everything you didn't want to know*

* Michael Snoyman
* VP of Engineering, FP Complete
* LambdaWorld 2017

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
<div><img src="/static/lambdaworld-logo.png" style="border:0;margin:0"><div>
</div>


---

## What's a monad transformer?

* (What's a monad? They're like burritos...)
* Adds extra functionality to an existing monad
* Convenient way to get this functionality
* Example: `ReaderT` avoids needing to pass an extra argument to functions
* I'll explain transformer _environment_ and _state_ during the talk

----

## Which transformers are we covering?

* `ReaderT`, `StateT`, `ExceptT`: covered explicitly
* `IdentityT`, `WriterT`, `LoggingT`, `MaybeT`: covered implicitly
    * They're isomorphic to something mentioned above
* Continuation-based transformers (`ContT`, `Conduit`): out of scope
    * Feel free to ask me about them later!
* Also, only doing shallow transformers (1 layer)

----

## Meet the transformers

```haskell
newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT  s m a = StateT  (s -> m (a, s))
newtype ExceptT e m a = ExceptT (     m (Either e a))
```

Or specialized to `IO` and turned into functions:

```haskell
type ReaderIO r a = r -> IO a
type StateIO  s a = s -> IO (a, s)
type ExceptIO e a =      IO (Either e a)
```

Let's motivate some problems

---

## A concurrent problem

*No trick question here*

I have `foo :: IO a` and `bar :: IO b`. I want to run both at the same
time in separate threads. How do I do that?

```haskell
-- In Control.Concurrent.Async
concurrently :: IO a -> IO b -> IO (a, b)
concurrently foo bar :: IO (a, b)
```

----

## An extra argument

Let's slightly modify things:

```haskell
foo :: MyEnv -> IO a
bar :: MyEnv -> IO b

baz :: MyEnv -> IO (a, b)
baz myEnv = concurrently (foo myEnv) (bar myEnv)
```

So far so good?

----

## What about ReaderT?

Explicit arguments are so boring! Let's move over to `ReaderT`.

```haskell
foo :: ReaderT MyEnv IO a
bar :: ReaderT MyEnv IO b

baz :: ReaderT MyEnv IO (a, b)
baz = concurrently foo bar -- bad!
```

* Now `concurrently` doesn't type check!
* Can we make this work anyway?

----

## Unwrap the `ReaderT`!

```haskell
concurrentlyR
  :: ReaderT env IO a
  -> ReaderT env IO b
  -> ReaderT env IO (a, b)
concurrentlyR (ReaderT foo) (ReaderT bar) =
  ReaderT $ \env ->
    concurrently
      (foo env)
      (bar env)
```

After all, `ReaderT` is just a convenient way to avoid argument
passing.

----

## Ask, lift, and run

* Don't need to use explicit data constructor unwrapping

```haskell
concurrentlyR foo bar = do
  env <- ask
  lift $ concurrently
    (runReaderT foo env)
    (runReaderT bar env)
```

* This all feels tedious, and unfortunately non-general
* _Leading question_ Surely there must be some general way to write
  `concurrently`, right?

----

## lifted-async

* Ask and ye shall receive!

```haskell
-- Control.Concurrent.Async.Lifted from lifted-async
concurrently
  :: MonadBaseControl IO m
  => m a
  -> m b
  -> m (a, b)
```

* `MonadBaseControl` is from `monad-control`
* Things that can be turned into `IO` and back... sort of
* Lots of instances, including `ReaderT`, `StateT` and `ExceptT`

---

## Pop quiz!

What is the output of this program?

```haskell
import Control.Monad.State.Strict
import Control.Concurrent.Async.Lifted

putter :: StateT Int IO ()
putter = do
  put 2
  concurrently_ (put 3) (put 4)

main :: IO ()
main = do
  res <- execStateT putter 1
  print res
```

* Outputs `4`
* What happened to `3`?

----

## Playing with bracket_

Guess the output (again)

```haskell
foo :: StateT [String] IO ()
foo = bracket_
  (modify (++ ["1"])) -- acquire
  (modify (++ ["3"])) -- release
  (modify (++ ["2"])) -- inner
main = do
  res <- execStateT foo []
  print res
```

Trick question! Depends on which `bracket_` you use

* lifted-base: `["2"]`
* exceptions: `["1","2","3"]`

Ahhhhh!!!

----

## Implement concurrently for StateT

Why does this happen? Let's implement `concurrently` for `StateT`

```haskell
concurrentlyS
  :: StateT s IO a
  -> StateT s IO b
  -> StateT s IO (a, b)
concurrentlyS (StateT f) (StateT g) = StateT $ \s0 -> do
  ((a, s1), (b, s2)) <- concurrently (f s0) (g s0)
  return ((a, b), s1)
```

We generated two states, and have to discard one of them!

----

## Implement bracket_ for StateT

```haskell
bracket_S
  :: StateT s IO a
  -> StateT s IO b
  -> StateT s IO c
  -> StateT s IO c
bracket_S (StateT f) (StateT g) (StateT h) = StateT $ \s0 ->
  bracket_ (f s0) (g s0) (h s0)
```

`h` doesn't see the new state from `f`, and `g` doesn't see the new
state from either `f` or `h`!

----

## How about `ExceptT`?

```haskell
concurrentlyE
  :: ExceptT e IO a
  -> ExceptT e IO b
  -> ExceptT e IO (a, b)
concurrentlyE (ExceptT f) (ExceptT g) = ExceptT $ do
  (ea, eb) <- concurrently f g
  return $ case (ea, eb) of
    (Right a, Right b) -> Right (a, b)
    (Left e, Right _) -> Left e
    (Right _, Left e) -> Left e
    (Left e, Left _discarded) -> Left e
```

More discarding!

---

## Take a step back

1. This is not just a bug in implementation
2. The ambiguity and discarding is inherent to implementing the algorithm
3. We cannot implement some classes of functions for some classes of
   transformers without discarding

Next: let's define those classes

----

## Control functions

* Arguably bad term, but it's used a lot
* Functions which take `IO`/`m` as arguments
* Aka contravariant in the monad
* Aka monad appears in negative position
* More info on nomenclature: https://www.fpcomplete.com/blog/2016/11/covariance-contravariance

---

## But does it lift?

Which of these functions can be converted to `StateT s IO` with `lift`?

```haskell
putStrLn :: String -> IO a
forkIO :: IO () -> IO ThreadId
catch :: Exception e => IO a -> (e -> IO a) -> IO a
try :: Exception e => IO a -> IO (Either e a)
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
```

----

## Monad transformer environment versus state

```haskell
newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT s m a  = StateT  (s -> m (a,s))
newtype ExceptT e m a = ExceptT (     m (Either e a))
```

* `ReaderT` has a transformer _environment_ (the `r`), but does not
  modify the output (`m a`)
* `ExceptT` has no environment, but has an output _state_ (`m (Either
  e a)` instead of `m a`)
* `StateT` has both (`s` as input, `m (a, s)` as output)

----

## Unlifting

* Also a made up term :)
* Unlifting is taking a control operation living in `IO` and moving it
  into a transformer
* Transformers with no monadic state can safely "unlift" control
  operations
* Transformers with monadic state may require discarding when
  unlifting

----

## ReaderT-like things

Any transformer without state is isomorphic to `ReaderT`. Examples:

* `IdentityT` (pretend `()` is the environment)
* `LoggingT` (the logging function is the environment)
* `NoLoggingT` (it's just a newtype on `IdentityT`)

---

## Unlifting without discarding

If a control function only takes one action as input, you can get away
without discarding.

```haskell
tryS (StateT f) = StateT $ \s -> do
  eres <- try (f s)
  return $
    case eres of
      Left e -> (Left e, s)
      Right (a, s') -> (Right a, s')
```

----

## Natural linear call path

Even though `catch` has two input actions, the handler is only called
_after_ the main action completes.

```haskell
catchS (StateT f) onErr = StateT $ \s ->
  f s `catch` (flip runStateT s . onErr)
```

No updated state is available from main action, since an exception was
thrown. This is safe!

----

## Finally a problem

Loses state updates in `g`:

```haskell
finallyS (StateT f) (StateT g) = StateT $ \s ->
  f s `finally` g s
```

Instead have to reimplement functionality:

```haskell
finallyS (StateT f) (StateT g) =
  StateT $ \s0 -> mask $ \restore -> do
    res <- try $ restore $ f s0
    case res of
      Left e -> do
        _ <- restore $ g s0
        throwIO (e :: SomeException)
      Right (s1, x) -> do
        (s2, _) <- restore $ g s1
        return (s2, x)
```

----

## The problem cases

Two categories of problem cases

1. Things like `finally`: can manually reimplement them to get the
   state retaining behavior desired. Problems:
     * End up with mismatched semantics between libraries (the
       `bracket_` example).
     * Tedious and error-prone to reimplement these functions.
2. Things which _cannot_ be solved, like `concurrently`

----

## Cheating

Could define a safe-for-`StateT` `bracket_`:

```haskell
bracket_
  :: MonadBaseControl IO m
  => IO a
  -> IO b
  -> m c
  -> m c
```

But it's not exactly the type signature people expect.

---

## Existing generic solutions

Two basic approaches today for typeclass-based control function
lifting.

----

## exceptions

Define an `mtl`-style typeclass for each operation.

```haskell
class Monad m => MonadThrow m where
  throwM :: Exception e => e -> m a
class MonadThrow m => MonadCatch m where
  catch :: Exception e => m a -> (e -> m a) -> m a
```

Need an extra typeclass for each operation (forking, timeout, etc).

----

## monad-control

Define a generic interface for all unlifting.

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
  type StM m a :: *
  liftBaseWith :: (RunInBase m b -> b a) -> m a
  restoreM :: StM m a -> m a
```

Difficult to understand, easy to write buggy instances, more likely to
implement bad discard behavior.

----

## unliftio

New entry in the market for control-like things

```haskell
class MonadIO m => MonadUnliftIO m where
  askUnliftIO :: m (m a -> IO a)
```

* Slightly different in practice (impredicativity...)
* Only has valid instances for `ReaderT`-like things
* Specialized to `IO` for simplicity
* Can do similar things with type hackery on `MonadBaseControl`
    * https://www.stackage.org/package/monad-unlift
    * Control.Concurrent.Async.Lifted.Safe

----

## Providing StateT and ExceptT features

But I want to have state and deal with failures! Practical
recommendations:

* Feel free to use any monad transformer "in the small," where you're
  not forking threads or acquiring resources
* Keep your overall applications to `ReaderT env IO` (or use `RIO`)

Prepare torches and pitchforks for the next two slides

----

## Use mutable variables

* `StateT` is inherently non-thread-safe
* It also doesn't allow state to survive a runtime exception
* Use a mutable variable and keep it in a `ReaderT`
* Choose the correct mutable variable based on concurrency needs
* Recommendation: default to `TVar` unless you have a good reason to
  do otherwise

----

## Use runtime exceptions

* If you're in `IO`, you have to deal with them anyway
* Less type safe than `ExceptT`? Yes
* But that's the Haskell runtime system
* Also, you have to deal with async exceptions anyway
* Caveat emptor: Many people disagree with me here

---

## Conclusion

* We like our `StateT` and `ExceptT` transformers
* We want to naturally lift functions into them
* It simply doesn't work in many cases
* Use libraries that don't silently discard your state
* You'll sometimes get stuck using less elegant things...
* But at least they work :)

----

## References

This talk is based on a series of blog posts. Get even more gory details!

* https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets
* https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
* https://www.fpcomplete.com/blog/2017/07/the-rio-monad
* https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library

----

## Questions?

Thanks everyone!
