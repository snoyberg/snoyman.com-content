tl;dr: Check
[this pull request](https://github.com/fpco/stackage/pull/3140/files)
to see if your package was just removed from Stackage Nightly.

Stackage Nightly maintains a set of upper bounds to give package
maintainers a grace period between dependencies updating their APIs
and users needing to support the new versions. Keeping these upper
bounds in place indefinitely places a burden on the rest of the
ecosystem needing to keep support for older versions of
packages. Therefore, the Stackage Curator team will periodically drop
these upper bounds, and in the process must temporarily drop some
packages.

Over the years, we've standardized on doing this drop immediately
following the release of a new major version of
[LTS Haskell](https://github.com/fpco/lts-haskell#readme). This allows
maximum packages to be included in an LTS release without imposing
"bleeding edge" requirements (something LTS Haskell tries to avoid
doing).

And as you may have guessed: I'm writing this now since I just dropped
a bunch of upper bounds and blocked a number of packages on Stackage
Nightly :). If you'd like to see if your package was evicted, please
[check out the relevant pull request](https://github.com/fpco/stackage/pull/3140/files). Some notes:

* The haskell-src-exts upgrade caused the most downstream breakage
* There were a _huge_ number of upper bounds for http-types. Since
  this is the second time a major version bump occurred recently, I
  left this upper bound in place. More information is
  [available on the http-types issue](https://github.com/fpco/stackage/issues/2976).

Once dependencies are fixed, please send pull requests to reenable
packages. Everyone is welcome to do so, whether you're unblocking your
own package or someone else's.

PS: Merry Christmas to all those celebrating today.
