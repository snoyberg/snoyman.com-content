I'm sure a number of readers have already seen something about the
situation around Stack and Stackage Nightly/GHC 8.2. I tried to
clarify how this happened on
[the relevant Github issue](https://github.com/commercialhaskell/stack/issues/3624),
plus the
[GHC trac ticket](https://ghc.haskell.org/trac/ghc/ticket/14558), but
thought I'd reshare as a blog post for others who are interested.

__EDIT__ Right after publishing, I saw that Stack 1.6.1 was released, so you
should probably just run `stack upgrade`. Keep reading if you're curious on the
bug.

## The problem

When the first releases of Stackage Nightly for GHC 8.2.1 started
coming out some months back, they did not work with Stack 1.5.0, due
to an issue with the `ghc.cabal` file on Hackage. The reason for this
is explained below. We made a point release (Stack 1.5.1) which worked
around the issue temporarily, until Stack 1.6 was released with the
complete fix.

In the interim, GHC 8.2.2 was released, and Stackage Nightly switched
over to it. Contrary to my initial claims: this was a _red herring_
and unrelated to anything.

On December 4, integer-gmp-1.0.1.0 was uploaded to Hackage, which
reintroduced all of the breakage we had with Stack 1.5.0. Since our
point release had a very targetted workaround (specifically for
`ghc.cabal`), it did not work around the same bug occurring for
`integer-gmp.cabal`. Therefore, all versions of Stack before 1.6 will
fail to build a Stackage release with GHC 8.2.

## The workaround

The best "workaround" is just a new release: Stack 1.6 was fortunately
already in release candidate mode, and as I type this up it's going
through the standard release process. By the time I hit publish, the
workaround may be to run `stack upgrade`.

If that's not the case, you can upgrade to the release candidate by
running:

    stack upgrade --binary-version 1.6.0.20171202

## Cabal background

In order to understand the explanation, you should be aware of a few
different things that are all called Cabal:

* cabal-install, the build tool. This is not relevant to the
  explanation below
* Cabal the library. This is a normal Haskell library which Stack
  depends on, and is used for (among other things) parsing cabal
  files.
* Cabal the file format. If you open up virtually any cabal file
  you'll see a `cabal-version: >= 1.10` looking field. This is stating
  which version of the Cabal file format is being used. New versions
  of Cabal-the-library may add new features to the Cabal file
  format. The version of the format tracks the library version it was
  released with, so that a cabal file stating `cabal-version: >= 1.24`
  can only be parsed by Cabal-the-library 1.24 or later.

There was an addition made to Cabal-the-file-format 2.0: a `^>=`
operator. This operator is not parseable by older versions of Cabal
the library (meaning: Cabal 1.24 or earlier). Stack 1.5 was built
against Cabal-the-library 1.24, and therefore cannot parse any Cabal
files using this new operator.

The Stackage build process prevents any such Cabal files from being
used yet to give tooling (like Stack) a chance to upgrade, something
I've requested of Hackage as well. However, there are some packages
which ship with GHC itself, and which Stackage has no control over in
the creation of a snapshot. This includes packages like `base`, `ghc`,
and `integer-gmp`.

## Original breakage

There's a short explanation (and some code to demonstrate it!) for the
original breakage with GHC 8.2.1 in the pull request:

<https://github.com/commercialhaskell/stack/pull/3304/files>

Prior to Stack 1.6, there was a bug where Stack would try to get some
metadata about libraries that shipped with GHC from their cabal files
instead of directly from the package database. Historically, this has
never been a problem, which is why it's survived in Stack for so
long. The reason is that, historically, GHC-shipped packages did not
use bleeding-edge features in their cabal files.

When GHC 8.2.1 was released, the `ghc.cabal` file uploaded to Hackage
did something new: it used a feature of the newly released Cabal 2.0
file format (the `^>=` operator) and required the new Cabal 2.0
library to parse it. This occurred before Stack had a chance to
upgrade to Cabal-the-library 2.0, and for that matter before
cabal-install 2.0 was released. In other words: at the time the file
was placed on Hackage, no officially released version of any common
tool supported it.

For unrelated reasons, I'd already fixed this bug on master as part of
a refactoring. Strangely enough, that refactoring had to do with
problems with revisions. Thanks to the revision system, it's not
possible to rely on cabal files on Hackage to tell you anything about
GHC-installed packages, since we can't know for certain which revision
was used to build the package. (We'll get to integer-gmp in a moment,
which is slightly worse in this regard.)

The behavior of Stack at this time with regard to GHC-shipped packages
was the following (and this is a bug):

* If the cabal file cannot be found: ignore the package entirely. This
  is necessary for packages like `rts`.
* If the cabal file is found: try to parse it, and fail if the parse
  fails.

It was this second bullet which caused a problem. When we discovered
this, we released an emergency patch release of Stack to work around
this situation and simply ignore parse failures from `ghc.cabal`. We
did not embark on a bigger fix because:

1. A bigger fix would involve much more code change, introducing the
   chance for regressions
2. We already had a fix on master, and knew that Stack 1.6 would be
   released before GHC 8.4

This went out the door, and all users who upgraded to Stack 1.5.1 were
able to use the new Stackage Nightly snapshots based on GHC 8.2.2.

## December 4, 2017

One of the packages that ships with GHC 8.2 is
`integer-gmp-1.0.1.0`. Until December 4, this package was not uploaded
to Hackage. As a result, Stack 1.5.1 simply ignored the package
entirely, which worked fine. However, something we didn't anticipate
happened:

* Months after the GHC 8.2.1 release, `integer-gmp-1.0.1.0` was
  uploaded to Hackage
* The cabal file that was uploaded was manually modified to use
  Cabal-the-format 2.0 features (again, the `^>=` operator).

You can compare the
[file on Hackage](http://hackage.haskell.org/package/integer-gmp-1.0.1.0/integer-gmp.cabal)
with the
[file on Github](https://github.com/ghc/ghc/blob/ghc-8.2.2-release/libraries/integer-gmp/integer-gmp.cabal). It's
unclear what the motivation was behind this modification, but this
modification is what broke Stack 1.5.1 and GHC 8.2.

Before this upload, the missing `integer-gmp.cabal` file was simply
ignored by Stack. Once it was uploaded, Stack (again, as a bug) tries
to parse it, fails, and gives up.

## The future

Obviously there was a bug in Stack that needed to be fixed, and has
been fixed. However, the irregularities around the `ghc.cabal` and
`integer-gmp.cabal` files are a little troubling, and make it
difficult to predict future behavior. Hopefully some new policies from
GHC HQ will address these concerns.

And while this case is a bug in Stack, I want to clarify a general
point. It is entirely expected that over time, older releases of Stack
will not be able to use newer Stackage snapshots. At some point in the
future, Stackage will allow Cabal 2.0-formatted cabal files into
snapshots, and then by design Stack 1.5 and earlier will be unable to
parse those files. That's unfortunate, but expected. What's unexpected
in this case was that

1. These cabal files slipped into a snapshot through the back door
   (GHC's package database) so quickly, before Stack 1.6 was out the
   door
2. That actions taken post-GHC release (a new upload of
   integer-gmp.cabal) could affect existing snapshots.

Both points will hopefully be hit both by the fixes that landed on
Stack 1.6 ensuring less eager parsing of cabal files, and changes in
GHC HQ policy.

## Summary

1. There's a bug in Stack, triggered by new behavior not seen before
   by GHC
2. That bug affects reproducibility, because an upload to Hackage in
   the future (or a revision for that matter) can break existing build
   plans
3. This bug is fixed on master fully (AFAICT, we've added an
   integration test to check for regressions)
4. Instead of putting out another emergency Stack 1.5 patch for
   integer-gmp.cabal, we're going to get Stack 1.6 out the door ASAP

I hope that clarifies. This is definitely an unfortunate situation,
and I know it's screwed up people's development, so my apologies on
that front. I hope for all our sakes (mine included!) that the
situation is more stable going forward.
