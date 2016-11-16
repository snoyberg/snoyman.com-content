I want to discuss two limitations in standard Haskell libraries around
concurrency, and discuss methods of improving the status quo. Overall,
Haskell's [concurrency](https://haskell-lang.org/library/async)
[story](https://haskell-lang.org/library/stm) is - in my opinion - the
best in class versus any other language I'm aware of, at least for the
single-machine use case. The following are two issues that I run into
fairly regularly and are a surprising wart:

* `putStrLn` is not thread-safe
* Channels cannot be closed

Let me back up these claims, and then ask for some feedback on how to
solve them.

## `putStrLn` is not thread-safe

The example below is, in my opinion, a prime example of beautiful
concurrency in Haskell:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async
import Control.Concurrent.Async
import Control.Monad (replicateM_)

worker :: Int -> IO ()
worker num = replicateM_ 5 $ putStrLn $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
    mapConcurrently worker [1..5]
    return ()
```

Well, it's beautiful until you see the (abridged) output:

```
Hi, HIiH'HH,imii , ,,I w  'IoIIm'r'' mkmmw e  owrwwro ookr#rrek2kkre
ee rrr# H  3#i##
4,51
```

Your mileage may vary of course. The issue here is that
`Prelude.putStrLn` works on `String`, which is a lazy list of `Char`s,
and in fact sends one character at a time to `stdout`. This is clearly
_not_ what we want. However, at the same time, many Haskellers -
myself included - consider `String`-based I/O a bad choice anyway. So
let's replace this with `Text`-based I/O:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Monad (replicateM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T

worker :: Int -> IO ()
worker num = replicateM_ 5
           $ T.putStrLn
           $ T.pack
           $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
    mapConcurrently worker [1..5]
    return ()
```

Unfortunately, if you run this (at least via `runghc`), the results
are the same. If you
[look at the implementation of `Data.Text.IO.hPutStr`](https://www.stackage.org/haddock/lts-7.9/text-1.2.2.1/src/Data.Text.IO.html#hPutStr),
you'll see that there are different implementations of that function
depending on the buffering straregy of the `Handle` we're writing
to. In the case of `NoBuffering` (which is the default with GHCi and
`runghc`), this will output one character at a time (just like
`String`), whereas `LineBuffering` and `BlockBuffering` have batch
behavior. You can see this with:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Monad (replicateM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

worker :: Int -> IO ()
worker num = replicateM_ 5
           $ T.putStrLn
           $ T.pack
           $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    mapConcurrently worker [1..5]
    return ()
```

While better, this still isn't perfect:

```
Hi, I'm worker #4Hi, I'm worker #5Hi, I'm worker #1


Hi, I'm worker #4Hi, I'm worker #5Hi, I'm worker #1


Hi, I'm worker #4Hi, I'm worker #5
```

Unfortunately, because
[newlines are written to stdout separately from the message](https://www.stackage.org/haddock/lts-7.9/text-1.2.2.1/src/Data.Text.IO.html#hPutStrLn),
these kinds of issues happen too frequently. This can be worked around
too by using `putStr` instead and manually appending a newline
character:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Monad (replicateM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

worker :: Int -> IO ()
worker num = replicateM_ 5
           $ T.putStr
           $ T.pack
           $ "Hi, I'm worker #" ++ show num ++ "\n"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    mapConcurrently worker [1..5]
    return ()
```

Finally, we can avoid the buffering-dependent code in the text package
and use `ByteString` output, which has the advantage of automatically
using this append-a-newline logic for small-ish `ByteString`s:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as S8

worker :: Int -> IO ()
worker num = replicateM_ 100
           $ S8.putStrLn
           $ S8.pack
           $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
    mapConcurrently worker [1..100]
    return ()
```

However, this has the downside of assuming a certain character
encoding, which may be different from the encoding of the `Handle`.

__What I'd like__ I would like a function `Text -> IO ()` which -
regardless of buffering strategy - appends a newline to the `Text`
value and sends the entire chunk of data to a `Handle` in a
thread-safe manner. Ideally it would account for character encoding
(though assuming UTF8 may be an acceptable compromise for most use
cases), and it would be OK if very large values are occassionally
compromised during output (due to the `write` system call not
accepting the entire chunk at once).

__What I'd recommend today__ In a number of my smaller
applications/scripts, I've become accustomed to defining a `say =
BS.hPutStrLn stdout . encodeUtf8`. I'm tempted to add this to a
library - possibly even `classy-prelude` - along with either
reimplementing `print` as `print = say . T.pack . show` (or providing
an alternative to `print`). I've also considered replacing the `putStrLn` in
`classy-prelude` with this implementation of `say`.

However, I'm hoping others have some better thoughts on this, because
I don't really find these solutions very appealing.

## Non-closable channels

Let's implement a very simple multi-worker application with
communication over a `Chan`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

worker :: Chan Int -> Int -> IO ()
worker chan num = forever $ do
    i <- readChan chan
    say $ pack $ concat
        [ "Worker #"
        , show num
        , " received value "
        , show i
        ]

main :: IO ()
main = do
    chan <- newChan
    mapConcurrently (worker chan) [1..5] `concurrently`
        mapM_ (writeChan chan) [1..10]
    return ()
```

(Yes, I used the aforementioned `say` function.)

This looks all well and good, but check out the end of the output:

```haskell
Worker #5 received value 8
Worker #3 received value 9
Worker #1 received value 10
Main: thread blocked indefinitely in an MVar operation
```

You see, the worker threads have no way of knowing that there are no more `writeChan` calls incoming, so they continue to block. The runtime system notes this, and sends them an async exception to kill them. This is [a really bad idea for program structure](https://www.fpcomplete.com/blog/2016/06/async-exceptions-stm-deadlocks) as it can easily lead to deadlocks. Said more simply:

![If you rely on exceptions for non-exceptional cases, you're gonna have a bad time](https://i.sli.mg/eX0QY1.jpg)

Instead, the workers should have some way of knowing that the channel
is closed. This is a common pattern in other languages, and one I
think we should borrow. Implementing this with STM isn't too bad
actually, and can easily have an `IO`-based API if desired:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<|>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

data TCChan a = TCChan (TChan a) (TVar Bool)

newTCChan :: IO (TCChan a)
newTCChan = atomically $ TCChan <$> newTChan <*> newTVar False

closeTCChan :: TCChan a -> IO ()
closeTCChan (TCChan _ var) = atomically $ writeTVar var True

writeTCChan :: TCChan a -> a -> IO ()
writeTCChan (TCChan chan var) val = atomically $ do
    closed <- readTVar var
    if closed
        -- Could use nicer exception types, or return a Bool to
        -- indicate if writing failed
        then error "Wrote to a closed TCChan"
        else writeTChan chan val

readTCChan :: TCChan a -> IO (Maybe a)
readTCChan (TCChan chan var) = atomically $
    (Just <$> readTChan chan) <|> (do
        closed <- readTVar var
        check closed
        return Nothing)

worker :: TCChan Int -> Int -> IO ()
worker chan num =
    loop
  where
    loop = do
        mi <- readTCChan chan
        case mi of
            Nothing -> return ()
            Just i -> do
                say $ pack $ concat
                    [ "Worker #"
                    , show num
                    , " received value "
                    , show i
                    ]
                loop

main :: IO ()
main = do
    chan <- newTCChan
    mapConcurrently (worker chan) [1..5] `concurrently` do
        mapM_ (writeTCChan chan) [1..10]
        closeTCChan chan
    return ()
```

Fortunately, this problem has a preexisting solution: the
[stm-chans package](https://www.stackage.org/package/stm-chans), which
provides closable and bounded channels and queues. Our problem above
can be more easily implemented with:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.23 --install-ghc runghc --package async --package text --package stm-chans
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

worker :: TMQueue Int -> Int -> IO ()
worker q num =
    loop
  where
    loop = do
        mi <- atomically $ readTMQueue q
        case mi of
            Nothing -> return ()
            Just i -> do
                say $ pack $ concat
                    [ "Worker #"
                    , show num
                    , " received value "
                    , show i
                    ]
                loop

main :: IO ()
main = do
    q <- newTMQueueIO
    mapConcurrently (worker q) [1..5] `concurrently` do
        mapM_ (atomically . writeTMQueue q) [1..10]
        atomically $ closeTMQueue q
    return ()
```

__What I'd like__ The biggest change needed here is just to get
knowledge of this very awesome `stm-chans` package out there
more. That could be with blog posts, or even better with links from
the `stm` package itself. A step up from there could be to include
this functionality in the `stm` package itself. Another possible
niceity would be to add a non-STM API for these - whether based on STM
or MVars internally - for more ease of use. I may take a first step
here by simply depending on and reexporting `stm-chans` from
`classy-prelude`.

__What I'd recommend__ Probably pretty obvious: use `stm-chans`!

Like the previous point though, I'm interested to see how other people
have approached this problem, since I haven't heard it discussed much
in the past. Either others haven't run into this issue as frequently
as I have, everyone already knows about `stm-chans`, or there's some
other solution people prefer.
