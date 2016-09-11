As many people are likely aware, monads (incorrectly) have a bad rap
in the programming community for being difficult to learn. A string of
extremely flawed monad tutorials based on analogies eventually led to
a
[blog post by Brent Yorgey](https://byorgey.wordpress.com/2009/01/12/abstractsion-intuition-and-the-monad-tutorial-fallacy/)
about the flaws of this analogy-based approach. And we've seen
[great](http://haskellbook.com/)
[learning](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)
[materials](https://haskell-lang.org/documentation) on Haskell and
monads.

However, I'm disappointed to see the analogy-based route
disappear. Based on a
[recent Twitter poll](https://twitter.com/snoyberg/status/773892889868963841)
I ran, which is held to the highest levels of scientific and
statistical scrutiny, it's obvious that there is very high demand for
a rigorous analogy-based monad tutorial. I claim that the flaw in all
previous analogy based tutorials is lack of strong pop culture
references. Therefore, I'm happy to announce the definitive guide to
monads: Monads are like Lannisters.

Spoiler alert: if you haven't completed book 5 or season 6 of Haskell
yet, there _will_ be spoilers.

## Prereqs

The examples below will all be Haskell scripts that can be run with
the Stack build tool. Please
[grab Stack to play along](https://haskell-lang.org/get-started). Copy-paste
the full example into a file like `foo.hs` and then run it with `stack
foo.hs`. (This uses Stack's
[script interpreter support](https://www.fpcomplete.com/blog/2016/08/bitrot-free-scripts).)

## Hear me roar

Many people believe that the Lannister house words are "A Lannister
always pays his debts" (or her debts of course). We'll get to that
line in a moment. This belief however is false: the _true_ Lannister
house words are **Hear me roar**. So let's hear a Lannister roar:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

roar :: IO ()
roar = putStrLn "Roar"

main :: IO ()
main = do
    roar -- Tywin
    roar -- Cersei
    roar -- Jaime
    roar -- Tyrion
```

Roaring is clearly an _output_, and therefore it makes sense that our
action is an `IO` action. But roaring doesn't really do much besides
making sound, so its return is the empty value `()`. In our `main`
function, we use `do` notation to roar multiple times. But we can just
as easily use the `replicateM_` function to replicate a monadic action
multiple times and discard (that's what the `_` means) the results:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Control.Monad (replicateM_)

roar :: IO ()
roar = putStrLn "Roar"

main :: IO ()
main = replicateM_ 4 roar
```

## Tyrion, the scholar

As we all know, Tyrion is a prolific scholar, consuming essentially
any book he can get his hands on (we'll discuss some other consumption
next). Fortunately, monads are there to back him up, with the `Reader`
monad. Let's say that Tyrion is doing some late night research on wine
production (epic foreshadowment) in various kingdoms, and wants to
produce a total:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.Reader
import Data.Maybe (fromMaybe)

type Kingdom = String
type WineCount = Int
type WineData = Map Kingdom WineCount

tyrionResearch :: Reader WineData Int
tyrionResearch = do
    mnorth <- asks $ Map.lookup "north"
    mriverlands <- asks $ Map.lookup "riverlands"
    return $ fromMaybe 0 mnorth + fromMaybe 0 mriverlands

main :: IO ()
main = print $ runReader tyrionResearch $ Map.fromList
    [ ("north", 5)
    , ("riverlands", 10)
    , ("reach", 2000)
    , ("dorne", 1000)
    ]
```

While Tyrion may have chosen inferior kingdoms for wine production, it
does not take away from the fact that the `Reader` type has allowed
him access to data without having to explicitly pass it around. For an
example this small (no dwarf joke intended), the payoff isn't
great. But `Reader` is one of the simplest monads, and can be quite
useful for larger applications.

## Tyrion, the drinker

Let's try another one. We all know that Tyrion also likes to _drink_
wine, not just count it. So let's use our `Reader` monad to give him
access to a bottle of wine.

_However_, unlike reading data from a book, drinking from a bottle
actually changes the bottle. So we have to leave our pure `Reader`
world and get into the `ReaderT IO` world instead. Also known as
_monad transformers_. (Unfortunately I don't have time now for a full
post on it, but consider: __Transformers: Monads in Disguise__.)

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Control.Monad (replicateM_)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent.Chan

data Wine = Wine
type Bottle = Chan Wine

drink :: MonadIO m => Bottle -> m ()
drink bottle = liftIO $ do
    Wine <- readChan bottle
    putStrLn "Now I'm slightly drunker"

tyrionDrinks :: ReaderT Bottle IO ()
tyrionDrinks = replicateM_ 10 $ ReaderT drink

main :: IO ()
main = do
    -- Get a nice new bottle
    bottle <- newChan

    -- Fill up the bottle
    replicateM_ 20 $ writeChan bottle Wine

    -- CHUG!
    runReaderT tyrionDrinks bottle
```

## A Lannister always pays his debts

What is a debt, but receiving something from another and then
returning it? Fortunately, there's a monad for that too: the `State`
monad. It lets us take in some value, and then give it back - perhaps
slightly modified.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Control.Monad.Trans.State

type Sword = String

forge :: State Sword Sword
forge = do
    "Ice" <- get
    -- do our Valyrian magic, and...

    -- Repay our debt to Brienne
    put "Oathkeeper"

    -- And Tywin gets a sword too!
    return "Widows Wail"

killNedStark :: IO Sword
killNedStark = do
    putStrLn "Off with his head!"
    return "Ice"

main :: IO ()
main = do
    origSword <- killNedStark

    let (forTywin, forBrienne) = runState forge origSword

    putStrLn $ "Tywin received: " ++ forTywin
    putStrLn $ "Jaime gave Brienne: " ++ forBrienne
```

Not exactly justice, but the types have been satisfied! A monad always
pays its debts.

## Throwing

Monads are also useful for dealing with exceptional cases:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Control.Exception
import Data.Typeable (Typeable)

data JaimeException = ThrowBranFromWindow
    deriving (Show, Typeable)
instance Exception JaimeException

jaime :: IO ()
jaime = do
    putStrLn "Did anyone see us?"
    answer <- getLine
    if answer == "no"
        then putStrLn "Good"
        else throwIO ThrowBranFromWindow

main :: IO ()
main = jaime
```

## Killing

And thanks to asynchronous exceptions, you can also kill other threads
with monads.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Typeable (Typeable)

data JaimeException = Defenestrate
    deriving (Show, Typeable)
instance Exception JaimeException

bran :: IO ()
bran = handle onErr $ forever $ do
    putStrLn "I'm climbing a wall!"
    threadDelay 100000
  where
    onErr :: SomeException -> IO ()
    onErr ex = putStrLn $ "Oh no! I've been killed by: " ++ show ex

jaime :: ThreadId -> IO ()
jaime thread = do
    threadDelay 500000
    putStrLn "Oh, he saw us"
    throwTo thread Defenestrate
    threadDelay 300000
    putStrLn "Problem solved"

main :: IO ()
main = do
    thread <- forkIO bran
    jaime thread
```

Exercise for the reader: modify `bran` so that he properly recovers
from that exception and begins warging instead. (WARNING:
[don't recover from async exceptions in practice](https://haskell-lang.org/library/safe-exceptions).)

## Exiting

You can also exit your entire process with monads.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

import System.Exit

tommen :: IO ()
tommen = do
    putStrLn "Oh, my dear wife!"
    exitFailure

main :: IO ()
main = tommen
```

## Passing the baton of reign

We've seen Lannisters pass the baton of reign from family member to family member. We've also seem them ruthlessly destroy holy insitutions and have their private, internal affairs exposed. As it turns out, we can do all of that with some explicit [state token](https://wiki.haskell.org/Evaluation_order_and_state_tokens) manipulation!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Prim
import GHC.Types

joffrey :: IO ()
joffrey = do
    putStrLn "Look at your dead father's head Sansa!"
    putStrLn "Oh no, poisoned!"

tommen :: IO ()
tommen = do
    putStrLn "I'm in love!"
    putStrLn "*Swan dive*"

cersei :: IO ()
cersei = undefined -- season 7 isn't out yet

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO f) = f

main :: IO ()
main = IO $ \s0 ->
  case unIO joffrey s0 of
    (# s1, () #) ->
      case unIO tommen s1 of
        (# s2, () #) ->
          unIO cersei s2
```

## Honorable mentions

There's much more to be done with this topic, and there were other
ideas besides Lannisters for this post. To give some mention for other
ideas:

* There's plenty of other Lannister material to work with (skinning
  animals, Jaime and Cersei affairs, or Tyrion proclivities). But I
  had to draw the line somewhere (both for length of post and topics I
  felt like discussing...). Feel free to comment about other ideas.
* One of my favorites: [monads are like onions](https://twitter.com/00Syssiphus/status/774240234766819332)
* Monads are [just your opinion, man](https://twitter.com/GabrielG439/status/774234282323697664)
* "they're like Gargoyles, they decorate the public facing interface
  of important landmarks in Haskell-land."
  [link](https://twitter.com/tritlo/status/774235047780032512)

Personally, I was going to go with a really terrible "monads are like
printers" based on them taking plain paper in and spitting out printed
pages, but Lannisters was a lot more fun. Also, I'm sure there's
_something_ great to be done with presidential candidates, at the very
least `throwTo opponent Insult`.

## Disclaimer

Since I'm sure _someone_ is going to get upset about this: YES, this
post is meant entirely as parody, and should not be used for actually
learning anything except some random details of Game of Thrones, and
perhaps a bit of Haskell syntax. Please use the learning materials I
linked at the beginning for actually learning about Haskell and
monads. And my real advice: don't actually "learn monads," just start
using Haskell and you'll pick them up naturally.
