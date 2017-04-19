In each of the following, which is the best version of the type
signature (assume `Monad m` context)?

```haskell
when :: Bool -> m () -> m ()
when :: Bool -> m a  -> m ()

mapM_ :: (a -> m ()) -> [a] -> m ()
mapM_ :: (a -> m b)  -> [a] -> m ()

runConduit :: ConduitM () Void m r -> m r
runConduit :: ConduitM i  o    m r -> m r
```

In each of these cases, the second, more generalized version of the
signature can easily be provided, but the question is, should it be? I
know of the following arguments:

Pro generalized:

* Why would we artificially limit the generality of our functions?
* Having to sprinkle a bunch of `void`s around is a code smell

Against generalized:

* It can allow accidental bugs to slip in by unintentionally ignoring
  values
* In some cases, the generalized version has
  [worse performance](http://www.snoyman.com/blog/2017/01/foldable-mapm-maybe-and-recursive-functions)

I don't think these points bring anything new to the table: it seems
to me that these trade-offs are fairly well understood, even if not
talked about explicitly often. The other thing I'd observe is that,
ignoring the issue of performance, this is just a rehashing of the
much more general argument of explicit vs implicit. We can all
acknowledge that with liberal application of `void` and similar
functions, it's always possible to rewrite code relying on the
generalized version to use the specialized version. The question comes
down to that annoying need for throwing in `void`.

How do we determine whether we should use generalized or specialized
functions? I'm starting to follow this procedure:

1. If there's a performance concern, let that take precedence. Having
   accidentally slower code due to a desire to make code shorter/more
   beautiful is a bad trade-off.
2. If there's no performance issue (like with `runConduit`), it's a
   completely subjective decision. The facts I'd look for are examples
   of bugs people run into with the generalized version.

On the bug front, I think it's important to point out that, in my
experience, the bugs are less likely to appear during initial code
writing, but during code review and refactoring. When you write
something like `when foo getLine`, you've probably gone through the
thought process "I'm just trying to give the user a chance to hit
&lt;ENTER>, and I'm intentionally ignoring whatever the user entered."
But during refactoring that may not be so obvious, and some ignored
user input may be surpring. By contrast, `when foo (void getLine)`
stands out more blatantly.

Finally, in comparing this to the general discussion of explicit vs implicit, I want to point out that there's no "one right answer" here. This falls into the same category of "do I define a typeclass/interface?", which is always a judgement call. You can give general guidelines, but I think we can all agree that both extremes:

* Never define a typeclass for any purpose
* Define every single function to be a member of a typeclass

Are ridiculous oversimplifications that we should not advocate. Same
thing applies here: there are times when a generalized type signature
makes sense, and times when it doesn't.

## Conduit

As an aside, if anyone is wondering where this random blog post came
from, while working on a presentation on Conduit and Yesod, I
revisited
[an issue from a few months ago](https://github.com/snoyberg/conduit/issues/283)
about deprecating older type synonyms
([PR now available](https://github.com/snoyberg/conduit/pull/307)),
and was reminded of the ongoing debate around which of the following
is the correct `runConduit` signature:

```haskell
1: ConduitM ()   Void m r -> m r -- today
2: ConduitM i    o    m r -> m r -- most general
3: ConduitM Void Void m r -> m r -- standardize on Void
4: ConduitM ()   ()   m r -> m r -- standardize on unit
```

The current situation of `()` for input and `Void` for output has been
around for a while, and originated with discussions around the
conduit/pipes dichotomy. (And in fact,
[pipes today has the same split](https://www.stackage.org/haddock/lts-8.11/pipes-4.3.2/Pipes.html#t:Effect).)
I'm not convinced that the split between input and output is a great
thing, so the arguments in favor of each of these signatures seem to
be:

1. Backwards compatibility
2. Generalize, avoiding any need for explicit conversion ever, and
   avoid the `()`/`Void` debate entirely
    * Note that we won't _really_ avoid the debate entirely, since
      other parts of conduit will still need to indicate "nothing
      upstream" or "nothing downstream"
3. Most explicit about what we're doing: we guarantee that there will
   never be any real values yielded from upstream, or yielded
   downstream. You can look through the conduit codebase for usages of
   `absurd` to see this play out.
4. More explicit, but less cognitive overhead of learning about
   `Void`.

I think for now I'm leaning towards (1), as backwards compat has been
a big guiding principle for me, but the debate is still raging for me.
