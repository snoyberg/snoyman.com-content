Vincent Hanquez and I have been playing a proverbial game of hot
potato over the past few months about writing this blog post. A
cabal-install bug recently came to my attention that finally pushed me
over the edge into writing this myself. I'll discuss that a bit later.

The basic claim here is simple:

* Supporting older GHCs together with new ones adds a real maintenance
  cost to a library.
* This is even higher if your goal is to make the code compile without
  warnings across all supported versions. You'll often end up
  resorting to CPP or weird import hacks to work around redundant
  imports.
* There's nothing wrong with maintaining a code base using an old
  version of tools and libraries. However, you should make sure that
  you are pinning your dependencies in any such project, and therefore
  missing out on the latest updates to a library is not a critical
  issue, excluding security patches.

Usually, when I drop support for an older version of GHC in a library,
I receive no complaints. Occasionally, if I do receive a concern about
it, it's from someone trying to maintain their own CI matrix with
older GHC support. I rarely, if ever, receive a complaint from someone
trying to actually use an older version of GHC _and_ the newest
version of my code.

Instead of spinning wheels to maintain compatibility on the off chance
that someone may desire bleeding-edge libraries with years-old
compilers, I recommend cutting out support for older GHCs, updating
your cabal files to reflect this decision, and keeping your CI build
matrix curated appropriately. Only if a user has a specific request
for a feature to work with an older GHC would I consider changing
direction on this.

A generally accepted rule of thumb is __three major GHC releases__. At
the time of writing, that would mean supporting GHC 8.0, 8.2, and
8.4. I recommend only supporting the latest minor version of each
line, which would mean GHC 8.0.2, 8.2.2, and 8.4.3.

## Updating cabal files

The most common method for specifying which GHC versions you support
is to use the version of the `base` library that ships with GHC. You
can use this [handy lookup table](https://www.snoyman.com/base) I put
together, which I made because I can never remember this
information. Using that table, if you decided "I want to support GHC
8.0.2 and later", you'd look up the corresponding base version, find
that it's 4.9.1.0, and add the following to your cabal file:

```
build-depends: base >= 4.9.1.0
```

(Or equivalent if using hpack/package.yaml.)

Even though this is the standard approach today, there are a few
problems with it:

* The meaning isn't immediately clear. Most people in my experience
  think about GHC versions, _not_ `base` versions. Someone reading
  that line will need to go look up what GHC version that corresponds
  to.
* Newer users may not understand that `base` is a special,
  non-upgradeable package, and not realize that this is pinning the
  GHC version. (Many bug reports and support requests about both
  cabal-install and Stack back up this claim.)
* At some point in the future, in theory, `base` may in fact be
  upgradeable, at which point this approach won't work.
* `base` library versions don't always bump with new versions of
  GHC. For example, both GHC 8.4.2 and 8.4.3 ship with
  `base-4.11.1.0`. If you wanted to state that you only support GHC
  8.4.3, you can't use the base approach.
* It's quite possible to write some code which is compatible with an
  older `base` library version, but depends on newer features of GHC,
  like a new language extension.

Therefore, I recommend taking a two-pronged approach for dropping
support for older GHC versions:

* Add a lower bound on `base` as mentioned above. It's the standard
  approach, and some tooling may depend on it.
* Add a stanza to your cabal file using cabal conditionals based on
  the GHC version, e.g:

```
if impl(ghc < 8.0.2)
  build-depends: unsupported-ghc-version > 1 && < 1
```

Originally, Vincent and I had discussed using `buildable: False`, but
that's not an option, because...

## Recently uncovered cabal-install bug

I received a bug report on Friday about my new release of `yaml`
causing Travis build breakages for GHC 7.10.3. Later this was
[opened as a Hackage Trustee issue](https://github.com/haskell-infra/hackage-trustees/issues/165),
where I found out that the same bug was being triggered by an earlier
http-client release.

It turns out that with all released versions of cabal-install, there's
a bug that works something like this: if a component is `buildable:
False`, then all of its dependencies are ignored. However, the
dependency solver will still select such a package as a library
dependency, even though the library component is marked as
non-buildable. Then, when trying to build this dependency,
`cabal-install` will (rightfully) complain that it cannot build a
package with no library or executables.

This bug has been fixed on cabal-install HEAD (and maybe the 2.2
maintenance branch? I'm not 100% sure). However, it's unreasonable to
expect people to upgrade to bleeding-edge versions of tooling. So
instead of the `buildable: False` approach, I've adapted the cabal
files in question to use an impossible-to-satisfy constraint, which
the dependency solver does better with:

```
if impl(ghc < 8.0.2)
  -- Disable building with GHC before 8.0.2.
  -- Due to a cabal bug, do not use buildable: False,
  -- but instead give it an impossible constraint.
  -- See: https://github.com/haskell-infra/hackage-trustees/issues/165
  build-depends: unsupported-ghc-version > 1 && < 1
```

The dependency solver realizes that it cannot find a version of the
(non-existent) `unsupported-ghc-version` package which is both greater
than and less than version 1, and so ignores this version of the
package when using GHC before 8.0.2. Problem solved!

I strongly recommend added a comment explaining this unusual
formulation, as it's not clear to a reader why such a constraint is
being added instead of the arguably clearer `buildable: False`.
