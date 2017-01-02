__NOTE__ This content originally appeared on [School of Haskell](
https://www.schoolofhaskell.com/user/snoyberg/general-haskell/basics/foldable-mapm-maybe-and-recursive-functions).

I've
[run into this issue myself](https://github.com/snoyberg/conduit/commit/11877684b3adb7ca422ae5000fab1ebeb3fbe142),
and seen others hit it too. Let's start off with some very simple
code:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc
sayHi :: Maybe String -> IO ()
sayHi mname =
    case mname of
        Nothing -> return ()
        Just name -> putStrLn $ "Hello, " ++ name

main :: IO ()
main = sayHi $ Just "Alice"
```

There's nothing amazing about this code, it's pretty straight-forward
pattern matching Haskell. And at some point, many Haskellers end up
deciding that they don't like the explicit pattern matching, and
instead want to use a combinator. So the code above might get turned
into one of the following:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc
import Data.Foldable (forM_)

hiHelper :: String -> IO ()
hiHelper name = putStrLn $ "Hello, " ++ name

sayHi1 :: Maybe String -> IO ()
sayHi1 = maybe (return ()) hiHelper

sayHi2 :: Maybe String -> IO ()
sayHi2 = mapM_ hiHelper

main :: IO ()
main = do
    sayHi1 $ Just "Alice"
    sayHi2 $ Just "Bob"
    -- or often times this:
    forM_ (Just "Charlie") hiHelper
```

The theory is that all three approaches (`maybe`, `mapM_`, and
`forM_`) will end up being identical. We can fairly conclusively state
that `forM_` will be the exact same thing as `mapM_`, since
[it's just `mapM_` flipped](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/src/Data-Foldable.html#forM_). So
the question is: will the `maybe` and `mapM_` approaches do the same
thing? In this case, the answer is yes, but let's spice it up a bit
more. First, the `maybe` version:


```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc exec -- ghc -with-rtsopts -s
import Control.Monad (when)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

printChars :: Int -> [Char] -> IO ()
printChars idx str = maybe (return ()) (\(c, str') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) str') (uncons str)

main :: IO ()
main = printChars 1 $ replicate 5000000 'x'
```

You can compile and run this by saving to a `Main.hs` file and running
`stack Main.hs && ./Main`. On my system, it prints out the following
memory statistics, which from the maximum residency you can see runs
in constant space:

```
   2,200,270,200 bytes allocated in the heap
         788,296 bytes copied during GC
          44,384 bytes maximum residency (2 sample(s))
          24,528 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)
```

While constant space is good, the usage of `maybe` makes this a bit
ugly. This is a common time to use `forM_` to syntactically clean
things up. So let's give that a shot:


```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc exec -- ghc -with-rtsopts -s
import Control.Monad (when, forM_)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

printChars :: Int -> [Char] -> IO ()
printChars idx str = forM_ (uncons str) $ \(c, str') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) str'

main :: IO ()
main = printChars 1 $ replicate 5000000 'x'
```

The code is arguablycleaner and easier to follow. However, when I run
it, I get the following memory stats:

```
   3,443,468,248 bytes allocated in the heap
     632,375,152 bytes copied during GC
     132,575,648 bytes maximum residency (11 sample(s))
       2,348,288 bytes maximum slop
             331 MB total memory in use (0 MB lost due to fragmentation)
```

Notice how max residency has balooned up from 42kb to 132mb! And if
you increase the size of the generated list, that number grows. In
other words: we have _linear_ memory usage instead of constant,
clearer something we want to avoid.

The issue is that the implementation of `mapM_` in `Data.Foldable` is
not tail recursive, at least for the case of `Maybe`. As a result,
each recursive call ends up accumulating a bunch of "do nothing"
actions to perform after completing the recursive call, which all
remain resident in memory until the entire list is traversed.

Fortunately, solving this issue is pretty easy: write a tail-recursive
version of `forM_` for `Maybe`:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc exec -- ghc -with-rtsopts -s
import Control.Monad (when)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

forM_Maybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
forM_Maybe Nothing _ = return ()
forM_Maybe (Just x) f = f x

printChars :: Int -> [Char] -> IO ()
printChars idx str = forM_Maybe (uncons str) $ \(c, str') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) str'

main :: IO ()
main = printChars 1 $ replicate 5000000 'x'
```

This implementation once again runs in constant memory.

There's one slight difference in the type of `forM_Maybe` and `forM_`
specialized to `Maybe`. The former takes a second argument of type `a
-> m ()`, while the latter takes a second argument of type `a -> m
b`. This difference is unfortunately necessary; if we try to get back
the original type signature, we have to add an extra action to wipe
out the return value, which again reintroduces the memory leak:

```haskell
forM_Maybe :: Monad m => Maybe a -> (a -> m b) -> m ()
forM_Maybe Nothing _ = return ()
forM_Maybe (Just x) f = f x >> return ()
```

Try swapping in this implementation into the above program, and once
again you'll get your memory leak.

## mono-traversable

Back in 2014, I raised this same issue
[about the mono-traversable library](https://github.com/snoyberg/mono-traversable/issues/28),
and ultimately decided to change the type signature of the `omapM_`
function to the non-overflowing demonstrated above. You can see that
this in fact works:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc exec --package mono-traversable -- ghc -with-rtsopts -s
import Control.Monad (when)
import Data.MonoTraversable (oforM_)

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

printChars :: Int -> [Char] -> IO ()
printChars idx str = oforM_ (uncons str) $ \(c, str') -> do
    when (idx `mod` 100000 == 0)
        $ putStrLn $ "Character #" ++ show idx ++ ": " ++ show c
    printChars (idx + 1) str'

main :: IO ()
main = printChars 1 $ replicate 5000000 'x'
```

As we'd hope, this runs in constant memory.
