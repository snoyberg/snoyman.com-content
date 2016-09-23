In a few different conversations I've had with people, the idea of
reskinning some of the surface syntax of the
[conduit library](https://github.com/snoyberg/conduit#readme) has come
up, and I wanted to share the idea here. I call this "reskinning"
since all of the core functionality of conduit would remain unchanged
in this proposal, we'd just be changing operators and functions a bit.

The idea here is: conduit borrowed the operator syntax of `$$`, `=$`
and `$=` from enumerator, and it made sense at the beginning of its
lifecycle. However, for quite a while now conduit has evolved to the
point of having a unified type for `Source`s, `Conduit`s, and `Sink`s,
and the disparity of operators adds more confusion than it may be
worth. So without further ado, let's compare a few examples of conduit
usage between the current skin:

```haskell
import Conduit
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = do
    -- copy files
    runResourceT $ CB.sourceFile "source.txt" $$ sinkFile "dest.txt"

    -- sum some numbers
    print $ runIdentity $ enumFromToC 1 100 $$ sumC

    -- print a bunch of numbers
    enumFromToC 1 100 $$ mapC (* 2) =$ takeWhileC (< 100) =$ mapM_C print
```

With a proposed reskin:

```haskell
import Conduit2
import qualified Data.Conduit.Binary as CB

main :: IO ()
main = do
    -- copy files
    runConduitRes $ CB.sourceFile "source.txt" .| sinkFile "dest.txt"

    -- sum some numbers
    print $ runConduitPure $ enumFromToC 1 100 .| sumC

    -- print a bunch of numbers
    runConduit $ enumFromToC 1 100 .| mapC (* 2) .| takeWhileC (< 100) .| mapM_C print
```

This reskin is easily defined with this module:

```haskell
{-# LANGUAGE FlexibleContexts #-}
module Conduit2
    ( module Conduit
    , module Conduit2
    ) where

import Conduit hiding (($$), (=$), ($=), (=$=))
import Data.Void (Void)

infixr 2 .|
(.|) :: Monad m
     => ConduitM a b m ()
     -> ConduitM b c m r
     -> ConduitM a c m r
(.|) = fuse

runConduitPure :: ConduitM () Void Identity r -> r
runConduitPure = runIdentity . runConduit

runConduitRes :: MonadBaseControl IO m
              => ConduitM () Void (ResourceT m) r
              -> m r
runConduitRes = runResourceT . runConduit
```

To put this in words:

* Replace the `$=`, `=$`, and `=$=` operators - which are all synonyms
  of each other - with the `.|` operator. This borrows intuition from
  the Unix shell, where the pipe operator denotes piping data from one
  process to another. The analogy holds really well for conduit, so
  why not borrow it? (We call all of these operators "fusion.")
* Get rid of the `$$` operator - also known as the "connect" or
  "fuse-and-run" operator - entirely. Instead of having this
  two-in-one action, separate it into `.|` and `runConduit`. The
  advantage is that no one needs to think about whether to use `.|` or
  `$$`, as happens today. (Note that `runConduit` is available in the
  conduit library today, it's just not very well promoted.)
* Now that `runConduit` is a first-class citizen, add in some helper
  functions for two common use cases: running with `ResourceT` and
  running a pure conduit.

The goals here are to improve consistency, readability, and intuition
about the library. Of course, there are some downsides:

* There's a slight performance advantage (not benchmarked recently
  unfortunately) to `foo $$ bar` versus `runConduit $ foo =$= bar`,
  since the former combines both sets of actions into one. We may be
  able to gain some of this back with GHC rewrite rules, but my
  experience with rewrite rules in conduit has been less than
  reliable.
* Inertia: there's a lot of code and material out there using the
  current set of operators. While we don't need to ever remove (or
  even deprecate) the current operators, having two ways of writing
  conduit code in the wild can be confusing.
* Conflicting operator: doing a
  [quick Hoogle search](https://www.stackage.org/lts-7.0/hoogle?q=.%7C)
  reveals that the parallel package already uses `.|`. We could choose
  a different operator instead
  ([`|.`](https://www.stackage.org/lts-7.0/hoogle?q=%7C.) for instance
  seems unclaimed), but generally I get nervous any time I'm defining
  new operators.
* For simple cases like `source $$ sink`, code is now quite a few keystrokes
  longer: `runConduit $ source .| sink`.

Code wise, this is a trivial change to implement. Updating docs to
follow this new convention wouldn't be too difficult either. The
question is: is this a good idea?
