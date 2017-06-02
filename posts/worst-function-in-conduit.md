This blog post addresses a long-standing FIXME in the
conduit-combinators documentation, as well as
[a question on Twitter](https://twitter.com/bitemyapp/status/860919651957710849). This
blog post will assume familiarity with the Conduit streaming data
library; if you'd like to read up on it first, please
[check out the tutorial](https://haskell-lang.org/library/conduit). The
full executable snippet is at the end of this blog post, but we'll
build up intermediate bits along the way. First, the
[Stack](https://haskell-lang.org/get-started) script header, import
statement, and some minor helper functions.

```haskell
#!/usr/bin/env stack
--stack --resolver lts-8.12 script
import Conduit

src10 :: Monad m => ConduitM i Int m ()
src10 = yieldMany [1..10]

remaining :: MonadIO m => ConduitM i o m ()
remaining = lengthC >>= \x -> liftIO (putStrLn ("Remaining: " ++ show x))
```

`src10` just provides the numbers 1 through 10 as a source, and
`remaining` tells you how many values are remaining from
upstream. Cool.

Now let's pretend that the Conduit libraries completely forgot to
provide a `drop` function. That is, a function that will take an `Int`
and discard that many values from the upstream. We could write one
ourselves pretty easily:

```haskell
dropSink :: Monad m => Int -> ConduitM i o m ()
dropSink cnt
  | cnt <= 0 = return ()
  | otherwise = await >> dropSink (cnt - 1)
```

(Bonus points to readers: this function is inefficient in the case
that upstream has less than `cnt` values, optimize it.)

This function will drop a certain number of elements from upstream, so
the next component we monadically bind with can pick it up. Let's see
how that looks:

```haskell
goodDropSink :: IO ()
goodDropSink = runConduit
             $ src10
            .| (dropSink 5 >> remaining)
```

All well and good. But notice two things:

* I called this `dropSink`. Why sink?
* I stressed that we had to monadically bind. Why?

Well, there's another formulation of this drop function. Instead of
letting the next monadically bound component pick up remaining values,
we could _pass the remaining values downstream_. Fortunately it's
really easy to implement this function in terms of `dropSink`:

```haskell
dropTrans :: Monad m => Int -> ConduitM i i m ()
dropTrans cnt = dropSink cnt >> mapC id
```

(For more meaningless bonus points, feel free to implement this
without `dropSink`, or for a greater challenge, implement `dropSink`
in terms of `dropTrans`.) Anyway, this function can be used easily as:

```haskell
goodDropTrans :: IO ()
goodDropTrans = runConduit
              $ src10
             .| dropTrans 5
             .| remaining
```

Many may argue that this is more natural. To some extent, it mirrors
the behavior of `take` more closely, as `take` passes the initial
values downstream. On the other hand, `dropTrans` cannot _guarantee_
that the values will be removed from the stream; if instead of
`dropTrans 5 .| remaining` I simply did `dropTrans 5 .| return ()`,
then the `dropTrans` would never have a chance to fire, since
execution is driven from downstream. Also, as demonstrated, it's
really easy to capture this transformer behavior from the sink
behavior; the other way is trickier.

My point here is that we have two legitimate definitions of a
function. And from my experience, different people expect different
behavior for the function. In fact, some people (myself included)
intuitively expect different behavior _depending on the circumstance_!
This is what earns `drop` the title of worst function in conduit.

To make it even more clear how bad this is, let's see how you can
misuse these functions unintentionally.

```haskell
badDropSink :: IO ()
badDropSink = runConduit
            $ src10
           .| dropSink 5
           .| remaining
```

This code looks perfectly reasonable, and if we just replaced
`dropSink` with `dropTrans`, it would be correct. But instead of
saying, as expected, that we have 5 values remaining, this will
print 0. The reason: `src10` yields 10 values to
`dropSink`. `dropSink` drops 5 of those and leaves the remaining 5
untouched. But `dropSink` never itself yields a value downstream, so
`remaining` receives nothing.

Because of the type system, it's slightly trickier to misuse
`dropTrans`. Let's first do the naive thing of just assuming it's
`dropSink`:

```haskell
badDropTrans :: IO ()
badDropTrans = runConduit
             $ src10
            .| (dropTrans 5 >> remaining)
```

GHC does not like this one bit:

```
error:
    • Couldn't match type ‘Int’ with ‘Data.Void.Void’
      Expected type: ConduitM () Data.Void.Void IO ()
        Actual type: ConduitM () Int IO ()
```

The problem is that `runConduit` expects a pipeline where the final
output value is `Void`. However, `dropTrans` has an output value of
type `Int`. And if it's yielding `Int`s, so must `remaining`. This is
definitely an argument in favor of `dropTrans` being the better
function: the type system helps us a bit. (It's also an argument in
favor of keeping
[the type signature of `runConduit` as-is](http://www.snoyman.com/blog/2017/04/generalizing-type-signatures).)

However, it's still possible to accidentally screw things up in bigger
pipelines, e.g.:

```haskell
badDropTrans :: IO ()
badDropTrans = runConduit
             $ src10
            .| (dropTrans 5 >> remaining)
            .| (sinkList >>= liftIO . print)
```

This code may look a bit contrived, but in real-world Conduit code
it's not at all uncommon to deeply nest these components in such a way
that the error would not be present. You may be surprised to hear that
the output of this program is:

```
Remaining: 0
[6,7,8,9,10]
```

The reason is that the `sinkList` is downstream from `dropTrans`, and
grabs all of its output. `dropTrans` itself will drain all output from
`src10`, leaving nothing behind for `remaining` to grab.

The Conduit libraries use the `dropSink` variety of function. I wish
there was a better approach here that felt more intuitive to
everyone. The closest I can think of to that is deprecating `drop` and
replacing it with more explicitly named `dropSink` and `dropTrans`,
but I'm not sure how I feel about that (feedback welcome, and other
ideas _certainly_ welcome).

* * *

Full code

```haskell
#!/usr/bin/env stack
--stack --resolver lts-8.12 script
import Conduit

dropSink :: Monad m => Int -> ConduitM i o m ()
dropSink cnt
  | cnt <= 0 = return ()
  | otherwise = await >> dropSink (cnt - 1)

dropTrans :: Monad m => Int -> ConduitM i i m ()
dropTrans cnt = dropSink cnt >> mapC id

src10 :: Monad m => ConduitM i Int m ()
src10 = yieldMany [1..10]

remaining :: MonadIO m => ConduitM i o m ()
remaining = lengthC >>= \x -> liftIO (putStrLn ("Remaining: " ++ show x))

goodDropSink :: IO ()
goodDropSink = runConduit
             $ src10
            .| (dropSink 5 >> remaining)

badDropSink :: IO ()
badDropSink = runConduit
            $ src10
           .| dropSink 5
           .| remaining

goodDropTrans :: IO ()
goodDropTrans = runConduit
              $ src10
             .| dropTrans 5
             .| remaining

badDropTrans :: IO ()
badDropTrans = runConduit
             $ src10
            .| (dropTrans 5 >> remaining)
            .| (sinkList >>= liftIO . print)

main :: IO ()
main = do
  putStrLn "Good drop sink"
  goodDropSink
  putStrLn "Bad drop sink"
  badDropSink
  putStrLn "Good drop trans"
  goodDropTrans
  putStrLn "Bad drop trans"
  badDropTrans
```

Full output

```
Good drop sink
Remaining: 5
Bad drop sink
Remaining: 0
Good drop trans
Remaining: 5
Bad drop trans
Remaining: 0
[6,7,8,9,10]
```
