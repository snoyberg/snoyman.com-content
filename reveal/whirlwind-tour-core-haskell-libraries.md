---
title: Whirlwind Tour of Core Haskell Libraries
---

# Whirlwind Tour of Core Haskell Libraries

* Michael Snoyman
* LambdaConf Winter Retreat 2018

---

## Today's format

* Very informal
    * Interrupt!
    * Ask questions!
* References to lots of external learning material
* Can go into depth on any of that if desired
* Happy to discuss the topics here at length
* Mini hackathon as well? <https://github.com/snoyberg/codename-karka>

---

## Haskell's standard library

* Standard library is `base`
* Includes standard prelude, `Prelude`
* They both suck :(
    * Missing lots of functionality
    * Dangerous functions
* Need to call out to other libraries for almost any program

----

## Downsides to weak base

* Which library to use for this functionality?
* Dependency fear! I want my package to be lightweight
* Mismatches in core types across ecosystem

----

## Bonus problem: patterns

* Other languages have "design patterns"
* We don't need that in Haskell because types
* Except: how do you handle effects like possible failure, or HTTP
  calls?
    * Throw it all in `IO`!
    * Concrete monad transformers
    * `mtl`-style typeclasses
    * Effect libraries...
* Weak standard library => non-standard types => many different patterns

----

## End result

* Difficult for people new to the language to get started
* Lack of standardization across team makes code bases difficult to
  maintain
* Fear of dependencies ultimately leads to lots of reinvented
  functionality
    * Code bloat
    * More bugs

----

## Today's topic

* Cover a number of recommended libraries
    * Recommended by whom? Me :)
* Discuss some best practices for putting projects together
* Describe a new initiative to help bring this all together
* Finally: how to help Haskell take over the world!

---

## Features to cover

* Data structures
* I/O
* Concurrency
* Mutable data
* Exception handling
* External processes

Doesn't cover all needs, but most real programs will need almost all
of these.

----

## Libraries

- base
- bytestring
- containers
- deepseq
- directory
- filepath
- hashable
- microlens
- mtl
- text
- time
- typed-process
- unliftio
- unordered-containers
- vector

---

## Data structures

Three categories

* Sequential data
* Map/Dictionary
* Set

Sequential data the most complicated, let's knock out the other two

---

## Maps

* Three core datatypes
    * `data Map key value`
    * `data IntMap value`
    * `data HashMap key value`
* `IntMap` is a specialized, optimized `Map Int`
* `Map` is a binary tree, `HashMap` is (surprise) hash map
* `Map` requires `Ord` on keys, `HashMap` requires `Hashable` and `Eq`
* Generally: `HashMap` performs better

----

## Strict or lazy values

* Maps are always strict in their keys
    * Forcing a `Map` requires forcing all of its keys
* You _can_ be lazy in the values if you want...
* Usually: don't do that, use `Data.Map.Strict` et al

----

## Mutability

* Unlike other languages, `Map`s are immutable
* Less used hashtables library provides in place mutation
* Immutable is nice: don't worry about data races
* Stick it inside a `TVar`, `IORef`, etc
* Downside: performance is not as good

----

## Map API Overview

```haskell
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.HashMap.Strict as HashMap

singleton :: k -> v -> Map k v
fromList :: [(k, v)] -> Map k v
toList :: Map k v -> [(k, v)]
lookup :: k -> Map k v -> Maybe v
insert :: k -> v -> Map k v -> Map k v
insertWith :: (v -> v -> v) -> k -> v -> Map k v -> Map k v
union :: Map k v -> Map k v -> Map k v
unionWith :: (v -> v -> v) -> Map k v -> Map k v -> Map k v
```

What about duplicates?

----

## Sets

* Just like `Map`s, but no values (or `()` is the value...)
* No strict vs lazy difference... no values!
* No worry about duplicate keys... no values!

----

## Set API Overview

```haskell
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.HashSet as HashSet

singleton :: k -> Set k
fromList :: [k] -> Set k
toList :: Set k -> [k]
member :: k -> Set k -> Bool
insert :: k -> Set k -> Set k
union :: Set k -> Set k -> Set k
```

----

## Calculating frequency

```haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  lbs <- BL.getContents
  let add m w = Map.insertWith (+) w 1 m
  mapM_ print $ Map.toList $ BL.foldl' add Map.empty lbs
```

----

## More information

* https://haskell-lang.org/library/containers

---

## Sequential data

Everyone knows lists, right?

```haskell
(++) :: [a] -> [a] -> [a]
concat :: [[a]] -> [a]
map :: (a -> b) -> [a] -> [b]
break :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int -> [a] -> ([a], [a])
null :: [a] -> Bool
length :: [a] -> Int
reverse :: [a] -> [a]
intercalate :: [a] -> [[a]] -> [a]
foldl' :: (b -> a -> b) -> b -> [a] -> b
and :: [Bool] -> Bool
sum :: Num a => [a] -> a
replicate :: Int -> a -> [a]
```

----

## Lists: the good

* Polymorphic on any contained value
* Lazy/infinite
* Cheap prepend (singly linked list)
* Pure data structure
* Built in syntactic sugar
* Easy to pattern match

----

## Lists: the bad

* Lots of memory overhead
    * Data constructor per cons
    * Pointer to value (1 word)
    * Pointer to rest of list (1 word)
* O(n) indexing
* Can hide bottom values (`1:2:undefined`)
* Consider overhead of `[Word8]` and `[Char]`

What do?

----

## Other languages

Most languages have a few sequential data types

* Linked list/doubly linked lists
* Queue/double-ended queue
* Array/vector

----

## Haskell's plethora

* Lists/difference lists
* Seq
* Arrays (don't bother, use vector)
* Vector: boxed, storable, unboxed
* ByteString: strict, lazy
* Text: strict, lazy
* ShortByteString (seriously?)

Why so many?

----

## Haskell's memory model

We have four ways of storing sequential data

* Entirely as normal heap objects (list, Seq, diff lists)
* Primitive boxed arrays (boxed vector)
* Unpinned memory (Text, ShortByteString, unboxed vector)
* Pinned memory (ByteString, storable vector)

----

## Heap objects

* Pointers, pointers everywhere
* Memory overhead for the allocations/GC
* CPU overhead for following pointers

----

## Primitive boxed arrays

* Packed representation of pointers
* Still follow pointers to the values
* Pointers can point to thunks, which is why they're value lazy
* Less pointer overhead, but still some
* Allows _any heap object_ to be stored

----

## Unpinned memory

* Byte array managed by garbage collector
* GC can move it around
    * Reducing fragmentation
    * Can't pass it over FFI
* Values stored as bytes
    * Must be representable as bytes
    * Must represent in fixed size
    * Cannot be lazy

----

## Pinned memory

* Standard `malloc`-style buffers
* GC __can't__ move it around
    * Can fragment memory (don't hold for too long)
    * Can pass it over FFI
* Values stored as bytes, same as unpinned

----

## Haskell's laziness

Three levels of laziness in these data structures

* Fully lazy, both the values and the structure itself are lazy (e.g.,
  list `oo:bar:undefined`)
* Spine strict: values can be lazy, but not the structure (e.g., boxed
  vector, `fromList [undefined]`)
* Fully strict: nothing lazy (e.g., `ByteString`, this fails `fromList
  [undefined]`)
* Semi-strict: lazy list of strict chunks (lazy `ByteString` and `Text`)

----

## Overlaps

That still leaves us with some overlaps

* List vs diff list vs `Seq`: different time complexity for some
  operations
* `ShortByteString` vs unboxed `Vector Word8`: same thing
* `ByteString` vs storable `Vector Word8`: also same thing
* `Text` does not overlap: it's a `ShortByteString` containing UTF-16
  codepoints with a `Char`-based API

----

## What to use?

* `ShortByteString`: smaller and long lived
* `ByteString` interacting with FFI (I/O)
* `Text` for storing textual values
* If it works and not FFI: unpinned vector
* If it works _and_ you need FFI: storable vector
* Need spine laziness (e.g., infinite), use lists
* Unusual optimizations
    * Cheap append _and_ inspection: `Seq`
    * Cheap append: difference lists
* Otherwise: boxed vector

----

## The string problem

* Lots of theoretical ways to represent string-like stuff
* `[Char]`, strict/lazy `ByteString`/`Text`, `ShortByteString`,
  `Vector` of `Word8` or `Char`...
* `Vector` of `Char` is always a bad idea: too much memory
* Good that we have bytes vs text difference
* Need to use `Text` instead of `String` everywhere... not there yet
* Conclusion: use strict `ByteString` or `Text` almost everywhere,
  convert when necessary

----

## Why not use...

* Lazy `ByteString` or `Text`
    * Useful sometimes, like lazily generating large data
    * Mostly used for lazy I/O, which I advise against (use conduit
      instead)
* Unboxed/storable vectors instead of `ByteString`/`ShortByteString`
    * Hysterical raisins, probably the right thing
    * Lots of Rust envy here :(
* Lists everywhere
    * Performance

----

## The good news

* You know lists, right?
* You basically know all of these data structures
* Don't get overwhelmed with the choices, just follow the advice above

----

## Qualified imports (the bad news?)

* Since these all have similar APIs, names conflict
* Use qualified imports
* Recommended naming

```haskell
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V -- boxed
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Data.ByteString.Short
  (ShortByteString, toShort, fromShort)
```

----

## Further reading

* https://haskell-lang.org/library/vector
* https://haskell-lang.org/tutorial/string-types

----

## I/O

* Not the monad, actual input and output
* Console
* Files
* Network
* Streaming and in memory

----

## The bad: character I/O

* Implicit character decoding
* Newline handling
* Environment variables affect things
* For console: probably right
* File I/O: probably wrong
* Network I/O: lol nope

https://www.snoyman.com/blog/2016/12/beware-of-readfile

----

## The bad: lazy I/O

* Lazy `readFile` (et al)
    * Hides exceptions till later
    * Keeps file descriptors open longer than expected
* Answer: use strict I/O operations
* Dealing with large data? Use conduit
* Exception to the rule: lazy `writeFile` is fine (not actually lazy
  I/O)

----

## Reading a file

* Wants bytes? `Data.ByteString.readFile`
* Want text? Choose an encoding!
    * `decodeUtf8With lenientDecode <$> B.readFile fp`
* Let's get crazy

```haskell
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 fp = do
  bs <- readFileBinary fp
  case decodeUtf8' bs of
    Left e -> throwIO $ ReadFileUtf8Exception fp e
    Right text -> return text
```

----

## Writing a file

* No need to worry about char enc problems
* `Data.ByteString.writeFile`
* Have text? Choose an encoding!
    * `B.writeFile fp $ encodeUtf8 text`

----

## Copy a file

What's wrong with this code?

```haskell
bs <- B.readFile inputFile
B.writeFile outputFile bs
```

----

## Streaming

Here's a conduit solution

```haskell
runConduitRes $ sourceFile inputFile
             .| sinkFile outputFile
```

Or without `ResourceT`:

```haskell
withSourceFile inputFile $ \src ->
withSinkFile outputFile $ \sink ->
  runConduit $ src .| sink
```

Why with pattern? We'll talk exceptions later

---

## Generating large output

What's wrong with this code?

```haskell
odds :: [Int]
odds = [1, 3..]

toLine :: Int -> String
toLine i = show i ++ "\n"

toLines :: [Int] -> String
toLines = foldr (\i rest -> toLine i ++ rest) ""

main :: IO ()
main = putStr $ toLines $ take 1000 odds
```

__Strings!__

----

## Strict ByteString

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Monoid ((<>))

odds = [1, 3..]

toLine :: Int -> ByteString
toLine i = B8.pack (show i) <> "\n"

toLines :: [Int] -> ByteString
toLines = foldr (\i rest -> toLine i <> rest) B8.empty

main = B8.putStr $ toLines $ take 1000 odds
```

Problem? Quadratic complexity

----

## Lazy ByteString

Avoid the buffer copies

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Monoid ((<>))

odds = [1, 3..]

toLine :: Int -> ByteString
toLine i = BL8.pack (show i) <> "\n"

toLines :: [Int] -> ByteString
toLines = foldr (\i rest -> toLine i <> rest) BL8.empty

main = BL8.putStr $ toLines $ take 1000 odds
```

Still quadratic :(

----

## Builders

Single-copy data structure, efficient `Handle` interaction

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Builder (Builder, intDec, hPutBuilder)
import System.IO (stdout)
import Data.Monoid ((<>))

odds = [1, 3..]

toLine :: Int -> Builder
toLine i = intDec i <> "\n"

toLines :: [Int] -> Builder
toLines = foldr (\i rest -> toLine i <> rest) mempty

main = hPutBuilder stdout $ toLines $ take 1000 odds
```

----

## Text builders

* Text also has a builder
* Much less useful overall, no direct output capabilities
* Downside to ByteString builders: have to assume a character encoding
* For console, may be a problem, but great for network or file I/O

---

## Networking

* Low level libraries like network
    * Use the `Network.Socket` API!
* conduit-based helper functions on top of that
* WAI and Warp for web servers
* http-conduit for web clients
* Many other libraries out there too

----

## Web server

* Lots of details here: <https://github.com/fpco/applied-haskell/blob/master/web-services.md>
* Who wants to go down the rabbit hole?

----

## Web client

* <https://haskell-lang.org/library/http-client>
* Optional rabbit hole again

----

## Network server with conduit

Simple echo server example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import Data.Conduit.Network

main :: IO ()
main = runTCPServer (serverSettings 3001 "127.0.0.1") echo

echo :: AppData -> IO ()
echo app = runConduit $ appSource app .| appSink app
```

---

## Side adventure: unliftio

* Many more details on the motivation tomorrow
* Two packages
    * unliftio-core provides a typeclass
    * unliftio wraps a bunch of libraries with that type class
* If it's in unliftio: it's good to use, do it!
* Epic foreshadowment for tomorrow's presentation :)

---

## Mutable data

* https://github.com/fpco/applied-haskell/blob/master/mutable-variables.md
* Single cell references: https://www.stackage.org/package/mutable-containers

---

## Concurrency

* tl;dr: use `UnliftIO.Async` and `UnliftIO.STM`
* https://haskell-lang.org/library/stm
* https://haskell-lang.org/library/async

---

## Exception handling

* Documentation still out of date
* We'll cover the why of things tomorrow
* Short answer: use `UnliftIO.Exception`

---

## External processes

* https://haskell-lang.org/library/typed-process

---

## Structuring applications

* Use `RIO`
* Define `Has` typeclasses
* Demonstration: `pantry`: https://github.com/snoyberg/codename-karka/blob/master/pantry/src/Pantry.hs
* <https://github.com/commercialhaskell/stack/tree/rio/subs/rio>

---

## Random grab bag

* <https://wiki.haskell.org/Typeclassopedia>
* <https://haskell-lang.org/tutorial/operators>
* <https://haskell-lang.org/tutorial/synonyms>
* <https://haskell-lang.org/library/optparse-applicative>
