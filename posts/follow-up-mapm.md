This is a short follow-up to my
[blog post about mapM_ and Maybe](/blog/2017/01/foldable-mapm-maybe-and-recursive-functions). Roman
Cheplyaka [started a discussion](http://disq.us/p/1f5uz82) on that
post, and ultimately we came up with the following implementation of
`mapM_` which works for all `Foldable`s and avoids the
non-tail-recursive case for `Maybe` as desired:

```haskell
mapM_ :: (Applicative m, Foldable f) => (a -> m ()) -> f a -> m ()
mapM_ f a =
    go (toList a)
  where
    go [] = pure ()
    go [x] = f x -- here's the magic
    go (x:xs) = f x *> go xs
```

Why is this useful? If you implement `mapM_` directly in terms of
`foldr` or `foldMap`, there is no way to tell that you are currently
looking at the last element in the structure, and therefore will
always end up with the equivalent of `f x *> pure ()` in your expanded
code. By contrast, with explicit pattern matching on the list-ified
version, we can easily pattern match with `go [x]` and avoid `*> pure
()` bit, thereby making tail recursion possible.

Some interesting things to note:

* Using `() <$ f x` instead of `f x *> pure ()` or `f x >> return ()`
  seemed to make no difference for tail recursion purposes.
* As a result of that, we still need to have the `()`-specialized type
  signature I describe in the previous blog post, there doesn't seem
  to be a way around that.
* As you can see from the benchmark which I
  [unceremoniously ripped off from Roman](https://gist.github.com/snoyberg/2239e7601306371058ca0e5650dfcd2d),
  there do not appear to be cases where this version has more memory
  residency than `mapM_` from `base`. Roman had raised the concern
  that the intermediate list may involve extra allocations, though it
  appears that GHC is smart enough to avoid them.

Here are the results. Notice the significantly higher residency
numbers for `base`:

```
   5000      roman          36,064 bytes
   5000    michael          36,064 bytes
   5000       base          36,064 bytes
  50000      roman          36,064 bytes
  50000    michael          36,064 bytes
  50000       base         133,200 bytes
 500000      roman          44,384 bytes
 500000    michael          44,384 bytes
 500000       base       2,354,216 bytes
5000000      roman          44,384 bytes
5000000    michael          44,384 bytes
5000000       base      38,235,176 bytes
```

My takeaway from all of this: it's probably too late to change the
type signature of `mapM_` and `forM_` in `base`, but this alternative
implementation is a
[good fit for mono-traversable](https://github.com/snoyberg/mono-traversable/pull/121). Perhaps
there are some rewrite rules that could be applied in `base` to get
the benefits of this implementation as well.

* * *

Completely tangential, but: as long as I'm linking to pull requests
based on blog posts, I've
[put together a PR](https://github.com/snoyberg/mono-traversable/pull/120)
for classy-prelude and conduit-combinators that gets rid of
generalized I/O operations, based on my
[readFile blog post](http://www.snoyman.com/blog/2016/12/beware-of-readfile).
