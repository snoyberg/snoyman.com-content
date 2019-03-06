Right off the bat, the title of this blog post is ambiguous. In normal
Haskell usage, there are in fact 5 different, commonly used `readFile`
functions: for `String`, for strict and lazy `Text`, and for strict
and lazy `ByteString`. `String` and lazy `Text`/`ByteString` suffer
from problems of lazy I/O, which I'm not going to be talking about at
all today. I'm instead focused on the problems of character encoding
that exist in the `String` and strict/lazy `Text` variants. For the
record, these problems apply equally to `writeFile` and a number of
other functions.

__EDIT__ See the [end of this blog post](#real-world-failures), where
I've begun collecting real-life examples where this I/O behavior is
problematic.

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

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Have you ever needed the behavior of Prelude.readFile, which chooses its character encoding based on environment variables?</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/811624134174183424">December 21, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

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

-- Downloaded from: http://www.gutenberg.org/cache/epub/345/pg345.txt
fp :: FilePath
fp = "pg345.txt"

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

<img src="http://imagehost.cc/images/2019/03/06/bench.png" alt="Benchmark Results" style="max-width:100%">

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
time                 8.605 ms   (8.513 ms .. 8.720 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 8.719 ms   (8.616 ms .. 8.889 ms)
std dev              354.9 μs   (236.1 μs .. 535.2 μs)
variance introduced by outliers: 18% (moderately inflated)

benchmarking Data.Text.IO
time                 3.735 ms   (3.701 ms .. 3.763 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 3.703 ms   (3.680 ms .. 3.726 ms)
std dev              76.23 μs   (62.41 μs .. 97.13 μs)

benchmarking Data.Text.Lazy.IO
time                 2.995 ms   (2.949 ms .. 3.050 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 3.026 ms   (2.998 ms .. 3.071 ms)
std dev              109.3 μs   (81.86 μs .. 158.8 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking Data.ByteString.readFile
time                 218.1 μs   (215.1 μs .. 221.6 μs)
                     0.992 R²   (0.987 R² .. 0.996 R²)
mean                 229.2 μs   (221.0 μs .. 242.5 μs)
std dev              34.36 μs   (24.13 μs .. 48.99 μs)
variance introduced by outliers: 90% (severely inflated)

benchmarking Data.ByteString.Lazy.readFile
time                 162.8 μs   (160.7 μs .. 164.7 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 164.3 μs   (162.7 μs .. 165.9 μs)
std dev              5.481 μs   (4.489 μs .. 6.557 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking strict decodeUtf8
time                 1.283 ms   (1.265 ms .. 1.307 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 1.285 ms   (1.274 ms .. 1.303 ms)
std dev              46.84 μs   (35.27 μs .. 67.71 μs)
variance introduced by outliers: 25% (moderately inflated)

benchmarking strict decodeUtf8With lenientDecode
time                 1.298 ms   (1.287 ms .. 1.309 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.290 ms   (1.280 ms .. 1.298 ms)
std dev              29.77 μs   (24.26 μs .. 36.97 μs)
variance introduced by outliers: 11% (moderately inflated)

benchmarking lazy decodeUtf8
time                 589.2 μs   (581.3 μs .. 599.8 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 596.7 μs   (590.9 μs .. 605.2 μs)
std dev              22.43 μs   (16.87 μs .. 28.83 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking lazy decodeUtf8With lenientDecode
time                 598.1 μs   (591.0 μs .. 607.4 μs)
                     0.998 R²   (0.994 R² .. 0.999 R²)
mean                 594.7 μs   (588.3 μs .. 602.4 μs)
std dev              24.20 μs   (17.06 μs .. 39.94 μs)
variance introduced by outliers: 33% (moderately inflated)
```

<h2 id="real-world-failures">Real world failures</h2>

This is a list of examples of real-world problems caused by
the behavior described in this blog post. It's by far not an
exhaustive list, just examples that I've noticed. If you'd like to
include others here, please
[send me a pull request](https://github.com/snoyberg/snoyman.com-content/edit/master/posts/beware-of-readfile.md).

* [hpack failures on AppVeyor/Windows](https://github.com/sol/hpack/pull/142)
* [Unexpected failure with conduit+sinkHandle+Docker](https://www.reddit.com/r/haskell/comments/5nmmgv/how_to_pipe_unicode_to_a_process_using_conduit/)
* [stack new can fail when character encoding isn't
  UTF-8](https://github.com/commercialhaskell/stack/pull/2867) (and [mailing list discussion](https://groups.google.com/d/msg/yesodweb/ZyWLsJOtY0c/aejf9E7rCAAJ))
