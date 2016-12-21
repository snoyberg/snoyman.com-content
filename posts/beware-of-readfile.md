Right off the bat, the title of this blog post is ambiguous. In normal
Haskell usage, there are in fact 5 different, commonly used `readFile`
functions: for `String`, for strict and lazy `Text`, and for strict
and lazy `ByteString`. `String` and lazy `Text`/`ByteString` suffer
from problems of lazy I/O, which I'm not going to be talking about at
all today. I'm instead focused on the problems of character encoding
that exist in the `String` and strict/lazy `Text` variants. For the
record, these problems apply equally to `writeFile` and a number of
other functions.

Let's start off with a simple example. Try running the following
program:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 --install-ghc runghc --package text

main :: IO ()
main = writeFile "test.html"
    "<html><head><meta charset='utf-8'><title>Hello</title></head>\
    \<body><h1>Hello!</h1><h1>שלום!</h1><h1>¡Hola!</h1></body></html>"
```

For most people out there, this will generate a `test.html` file
which, when opened in your browser, displays the phrase Hello in
English, Hebrew, and Spanish. Now watch what happens when I run the
same program with a modified environment variable on Linux:

```
$ ./Main.hs
# Everything's fine
$ env LC_CTYPE=en_US.iso88591 ./Main.hs
Main.hs: test.html: commitBuffer: invalid argument (invalid character)
```

This behavior is
[very clearly documented in `System.IO`](https://www.stackage.org/haddock/lts-7.14/base-4.9.0.0/System-IO.html#g:23),
namely that GHC will follow system-specific rules to determine the
appropriate character encoding rules, and open up `Handle`s with that
character encoding set.

This behavior makes a lot of sense for the special standard handles of
`stdin`, `stdout`, and `stderr`, where we need to interact with the
user and make sure the console receives bytes that it can display
correctly. However, I'm also going to claim the following:

__In exactly 0 cases in my Haskell career have I desired the character
encoding guessing functionality of the textual `readFile` functions__

The reason is fairly simple: when reading and writing files, I'm
almost always dealing with some file format. The first code example
above demonstrates one example of this: writing an HTML file. JSON and
XML work similarly. Config files are another example where the
system's locale settings should be irrelevant. In fact, I'm hard
pressed to come up with a case where I _want_ the current behavior of
respecting the system's locale settings.

In my ideal world, we end up with the following functions for working
with files:

```haskell
readFile :: MonadIO m => FilePath -> m ByteString -- strict byte string, no lazy I/O!
readFileUtf8 :: MonadIO m => FilePath -> m Text
readFileUtf8 = fmap (decodeUtf8With lenientDecode) . readFile
-- maybe include a non-lenient variant that throws exceptions or
-- returns an Either value on bad character encoding
writeFile :: MonadIO m => FilePath -> ByteString -> m ()
writeFileUtf8 :: MonadIO m => FilePath -> Text -> m ()
writeFileUtf8 fp = writeFile fp . encodeUtf8
-- conduit, pipes, streaming, etc, can handle the too-large-for-memory
-- case
```

For those unaware: lenient here means that if there is a character
encoding error, it will be replaced with the Unicode replacement
character. The default behavior of `decodeUtf8` is to throw an
exception. I can save my (very large) concerns about that behavior for
another time.

Files are inherently binary data, we shouldn't hide that. We should
also make it convenient for people to do the very common task of
reading and writing textual data with UTF-8 character encoding.

Since a blog post is always better if it includes a meaningless benchmark, let me oblige. I put together an overly simple benchmark comparing different ways to read a file. The code:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-7.14 exec --package criterion -- ghc -O2
import           Criterion.Main
import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
import qualified Data.Text.Encoding       as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy.Encoding  as TL
import qualified Data.Text.Lazy.IO        as TLIO

-- Just ASCII data, a good UTF-8 corpus would be better!
fp :: FilePath
fp = "/usr/share/dict/words"

main :: IO ()
main = defaultMain
    [ bench "String" $ nfIO $ readFile fp
    , bench "Data.Text.IO" $ nfIO $ TIO.readFile fp
    , bench "Data.Text.Lazy.IO" $ nfIO $ TLIO.readFile fp
    , bench "Data.ByteString.readFile" $ nfIO $ S.readFile fp
    , bench "Data.ByteString.Lazy.readFile" $ nfIO $ L.readFile fp
    , bench "strict decodeUtf8" $ nfIO $ fmap T.decodeUtf8 $ S.readFile fp
    , bench "strict decodeUtf8With lenientDecode"
        $ nfIO $ fmap (T.decodeUtf8With lenientDecode) $ S.readFile fp
    , bench "lazy decodeUtf8" $ nfIO $ fmap TL.decodeUtf8 $ L.readFile fp
    , bench "lazy decodeUtf8With lenientDecode"
        $ nfIO $ fmap (TL.decodeUtf8With lenientDecode) $ L.readFile fp
    ]
```

To run this, I used:

```
$ ./bench.hs && ./bench --output bench.html
```

(Yes, you can run a Stack script to compile that script. I only
discovered this trick recently.)

Here are the graphical results, full textual results are available
below:

<img src="https://i.sli.mg/FMU0gm.png" alt="Benchmark Results" style="max-width:100%">

Unsurprisingly, `String` I/O is the slowest, and `ByteString` I/O is
the fastest (since no character encoding overhead is involved). I
found three interesting takeaways here:

* Lazy I/O variants were slightly faster, likely because there was no
  memory buffer copying involved
* Lenient and non-lenient decoding were just about identical in
  performance, which is nice to know
* As I predicted, the I/O functions from the `text` package are
  significantly slower than using `bytestring`-based I/O and then
  decoding. I found similar results in the
  [say package](https://github.com/fpco/say#readme), and believe it is
  inherent in `Handle`-based character I/O, which involves copying all
  data through a buffer of `Word32` values.

__My recommendation to all__: never use `Prelude.readFile`,
`Data.Text.IO.readFile`, `Data.Text.Lazy.IO.readFile`, or
`Data.ByteString.Lazy.readFile`. Stick with `Data.ByteString.readFile`
for known-small data, use a streaming package (e.g, [conduit](https://haskell-lang.org/library/conduit)) if your choice for large
data, and handle the character encoding yourself. And apply this to
`writeFile` and other file-related functions as well.

```
benchmarking String
open bench.html
time                 25.07 ms   (24.73 ms .. 25.42 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 24.86 ms   (24.67 ms .. 25.04 ms)
std dev              390.4 μs   (299.0 μs .. 515.3 μs)

benchmarking Data.Text.IO
time                 10.35 ms   (10.26 ms .. 10.44 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.42 ms   (10.34 ms .. 10.49 ms)
std dev              191.1 μs   (145.6 μs .. 263.1 μs)

benchmarking Data.Text.Lazy.IO
time                 8.555 ms   (8.451 ms .. 8.637 ms)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 8.687 ms   (8.624 ms .. 8.777 ms)
std dev              216.3 μs   (161.8 μs .. 316.2 μs)

benchmarking Data.ByteString.readFile
time                 678.4 μs   (673.6 μs .. 684.8 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 681.0 μs   (677.9 μs .. 688.8 μs)
std dev              16.84 μs   (8.769 μs .. 33.94 μs)
variance introduced by outliers: 15% (moderately inflated)

benchmarking Data.ByteString.Lazy.readFile
time                 548.6 μs   (539.7 μs .. 557.2 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 548.1 μs   (543.5 μs .. 553.0 μs)
std dev              15.77 μs   (12.96 μs .. 20.76 μs)
variance introduced by outliers: 21% (moderately inflated)

benchmarking strict decodeUtf8
time                 2.307 ms   (2.285 ms .. 2.327 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.315 ms   (2.301 ms .. 2.328 ms)
std dev              46.77 μs   (38.91 μs .. 60.47 μs)

benchmarking strict decodeUtf8With lenientDecode
time                 2.316 ms   (2.290 ms .. 2.347 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.314 ms   (2.301 ms .. 2.327 ms)
std dev              40.60 μs   (32.07 μs .. 52.18 μs)

benchmarking lazy decodeUtf8
time                 1.755 ms   (1.731 ms .. 1.780 ms)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 1.760 ms   (1.745 ms .. 1.778 ms)
std dev              57.04 μs   (46.54 μs .. 70.84 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking lazy decodeUtf8With lenientDecode
time                 1.790 ms   (1.748 ms .. 1.845 ms)
                     0.995 R²   (0.991 R² .. 0.999 R²)
mean                 1.771 ms   (1.756 ms .. 1.795 ms)
std dev              61.60 μs   (40.08 μs .. 107.8 μs)
variance introduced by outliers: 22% (moderately inflated)
```
