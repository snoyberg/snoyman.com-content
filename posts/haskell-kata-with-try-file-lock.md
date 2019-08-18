This is the first Haskell code kata I've put on this blog (to my knowledge). The idea is to present a self contained, relatively small coding challenge to solidify some skills with Haskell. If people like this and would like to see more, let me know. Caveat: these will almost certainly be supply driven. As I notice examples like this in my code, I'll try to extract them like this blog post.

OK, here's the story. The [`filelock`](https://www.stackage.org/package/filelock) library provides a set of functions for working with locked files. Some of these will block until a file lock is available. However, some will instead return a `Maybe` value and use `Nothing` to represent the case where a lock is not available.

What's interesting about this is the `withTryFileLock` function, which is a rare combination of the `bracket` pattern and potential failure. Its signature is:

```haskell
withTryFileLock
  :: FilePath
  -> SharedExclusive
  -> (FileLock -> IO a)
  -> IO (Maybe a)
```

The `FilePath` parameter says which file to try and lock. `SharedExclusive` says the type of lock to take. The third parameter is the action to perform with the file lock. That action will return an `IO a` action. Then, if the lock is taken, that `a` value ends up wrapped in a `Just` constructor and returned from `withTryFileLock`. If the lock failed, then `Nothing` is returned.

The thing is, there's an alternative function signature we could have instead, which would provide a `Maybe FileLock` to the inner action. It looks like this:

```haskell
withTryFileLock
  :: FilePath
  -> SharedExclusive
  -> (Maybe FileLock -> IO a)
  -> IO a
```

Why would you want one versus the other? It's not the topic I'm focusing on today, and it honestly doesn't matter that much. Here's the code kata:

**Implement the second version in terms of the first, and the first version in terms of the second.**

To complete these code kata:

1. Copy/paste the code snippet below into a file called `Main.hs`
2. Make sure you have [Stack installed](https://haskell.fpcomplete.com/get-started).
3. Make tweaks to `Main.hs`.
4. Run `stack Main.hs`.
5. If you get an error in step 4, go back to 3.
6. Congratulations, you've successfully fixed the program _and_ parsed my BASIC-esque goto statement!

Bonus points: generalize `version1` and `version2` to work in any [`MonadUnliftIO`](https://www.stackage.org/package/unliftio-core).

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-14.1 script
import System.FileLock (FileLock, SharedExclusive (..), withTryFileLock)

-- We've imported this function:
--
-- withTryFileLock
--   :: FilePath
--   -> SharedExclusive
--   -> (FileLock -> IO a)
--   -> IO (Maybe a)

-- | Implement this function by using the 'withTryFileLock' imported above.
version1
  :: FilePath
  -> SharedExclusive
  -> (Maybe FileLock -> IO a)
  -> IO a
version1 = _

-- | And now turn it back into the original type signature. Use the
-- 'version1' function we just defined above.
version2
  :: FilePath
  -> SharedExclusive
  -> (FileLock -> IO a)
  -> IO (Maybe a)
version2 = _

-- | Just a simple test harness
main :: IO ()
main = do
  version1 "version1.txt" Exclusive $ \(Just _lock) ->
    version1 "version1.txt" Exclusive $ \Nothing ->
    putStrLn "Yay, it worked!"

  Just _ <- version2 "version2.txt" Exclusive $ \_lock -> do
    Nothing <- version2 "version2.txt" Exclusive $
      error "Should not be called"
    pure ()
  putStrLn "Yay, it worked!"
```
