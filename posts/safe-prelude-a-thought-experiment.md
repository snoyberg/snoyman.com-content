This blog post is to share a very rough first stab at a new prelude I
played around with earlier this month. I haven't used it in any
significant way, and haven't spent more than a few hours on it
total. I wrote it because I knew it was the only way to get the idea
out of my head, and am sharing it in case anyone finds the idea
intriguing or useful.

The project is available
[on Github at snoyberg/safe-prelude](https://github.com/snoyberg/safe-prelude#readme),
and I've
[uploaded the Haddocks for easier reading](/static/safe-prelude/SafePrelude.html)
(though, be warned, they aren't well organized at all). The rest of
this post is just a copy of the `README.md` file for the project.

* * *

This is a thought experiment in a different point in the alternative
prelude design space. After my
[blog post on readFile](http://www.snoyman.com/blog/2016/12/beware-of-readfile),
I realized I was unhappy with the polymorphic nature of `readFile` in
[classy-prelude](https://www.stackage.org/package/classy-prelude). Adding
that with
[Haskell Pitfalls](http://lorepub.com/post/2016-12-17-Haskell-Pitfalls)
I've been itching to try something else. I have a lot of hope for the
[foundation project](https://github.com/haskell-foundation/foundation#readme),
but wanted to play with this in the short term.

## Choices

* No partial functions, period. If a function can fail, its return
  type must express that. (And for our purposes: `IO` functions with
  runtime exceptions are _not_ partial.)
* Choose best in class libraries and promote them. `bytestring` and
  `text` fit that bill, as an example. Full listing below.
* Regardless of the versions of underlying libraries, this package
  will always export a consistent API, so that CPP usage should be
  constrained to just inside this package.
* Use generalization (via type classes) when they are well
  established. For example: `Foldable` and `Traversable` yes,
  `MonoFoldable` no.

    * _Controversial_ Avoid providing list-specific functions. This
      connects to the parent point. Most of the time, I'd argue that
      lists are _not_ the correct choice, and instead a `Vector`
      should be used. There is no standard for sequence-like
      typeclasses (though many exist), so we're not going to
      generalize. But we're also not going to use a less efficient
      representation.

      I was torn on this, but decided in favor of leaving out
      functions initially, on the basis that it's easier to add
      something in later rather than remove it.
* Encourage qualified imports with a consistent naming scheme. This is
  a strong departure from classy-prelude, which tried to make it
  unnecessary to use qualified imports. I'll save my feelings about
  qualified imports for another time, this is just a pragmatic choice
  given the other constraints.
* Export any non-conflicting and not-discouraged names from this
  module that make sense, e.g. `ByteString`, `Text`, or `readIORef`.

## Libraries

This list may fall out of date, so check the `.cabal` file for a
current and complete listing. I'm keeping this here to include
reasoning for some libraries:

* `bytestring` and `text`, despite some complaints, are clearly the
  most popular representation for binary and textual data,
  respectively
* `containers` and `unordered-containers` are both commonly used. Due
  to lack of generalization, this library doesn't expose any functions
  for working with their types, but they are common enough that adding
  the dependency just for exposing the type name is worth it
* `safe-exceptions` hides the complexity of asynchronous exceptions,
  and should be used in place of `Control.Exception`
* `transformers` and `mtl` are clear winners in the monad transformer
  space, at least for now
* While young, `say` has been very useful for me in avoiding
  interleaved output issues
* Others without real competitors: `deepseq`, `semigroups`

Packages I considered but have not included yet:

* `stm` is an obvious winner, and while I use it constantly, I'm not
  convinced everyone else uses it as much as I do. Also, there are some
  questions around generalizing its functions (e.g., `atomically` could
  be in `MonadIO`), and I don't want to make that decision yet.

    * `stm-chans` falls into this category too

* `async` is an amazing library, and in particular the `race`,
  `concurrently`, and `Concurrently` bits are an easy win. I've left
  it out for now due to questions of generalizing to
  `MonadBaseControl` (see `lifted-async` and its `.Safe` module)
* Similar argument applies to `monad-unlift`
* I didn't bother with exposing the `Vector` type... because which one
  would I expose? The `Vector` typeclass? Boxed `Vector`? Unboxed? I
  could do the classy-prelude thing and define `type UVector =
  Data.Vector.Unboxed.Vector`, but I'd rather not do such renamings.

## Qualified imports

Here are the recommend qualified imports when working with safe-prelude.

```haskell
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Lazy as BL
import qualified "text" Data.Text as T
import qualified "text" Data.Text.Lazy as TL
import qualified "containers" Data.Map.Strict as Map
import qualified "containers" Data.Set as Set
import qualified "unordered-containers" Data.HashMap.Strict as HashMap
import qualified "unordered-containers" Data.HashSet as HashSet
```
