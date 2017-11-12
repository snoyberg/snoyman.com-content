I'll start with the specific case I've seen pop up a few times
recently, and then expand to the general. If you're a package author
who has been affected by this, please note: I'm putting this
information into a blog post since it's easier to state this once and
link to it rather than rewrite an explanation on lots of different bug
trackers.

[hlint](https://www.stackage.org/package/hlint) is a great tool for
getting advice on improving your Haskell codebase (another great Neil
Mitchell product). And as such tools go, hlint has new versions which
improve its ability to provide useful advice. This means that,
sometimes, code which triggered no hlint warnings previously may
suddenly present with such warnings under a new hlint version.

Twice recently in my Stackage curation, I've seen a number of test
suites fail, even though the code for those packages was
unmodified. It turns out that the upgrade to a new version of hlint
caused a previously successful test suite to now fail. Clearly the
code isn't suddenly broken because a new version of hlint has been
released, but as far as the diagnostics of test suite failures are
concerned, that's exactly what happened.

## Recommendation

I do strongly recommend projects use hlint to get code
improvements. And I've seen some great results with using it as part
of the CI process, such as on Stack. (For the record: it wasn't my
idea and I didn't implement it. I was just pleasantly surprised when
my PRs failed because I had some style errors.) However, making the
test suite for the entire package fail because of a new version of
hlint is too much. Therefore:

*   __DO__ Have some way to run hlint from your CI process, if you
    want these warnings to block PRs. There are two approaches I can
    think of:

    * The way Stack does it: have a
      [separate part of the build matrix](https://github.com/commercialhaskell/stack/blob/46121be1b96465f1164e3f84cafa19c7369da9cc/.travis.yml#L39)
      just for style errors. The cabal file for the project itself
      knows nothing about hlint.
    * Via a test suite in your cabal file which is disabled by
      default. Then: turn on that test suite with a flag from your CI
      configuration.

*   __DON'T__ Set up your package which is uploaded to Hackage/built
    by Stackage such that it will fail if a style-based error occurs.

## General recommendation

The general takeaway from this is: when you're building your code on
CI, be as strict as you want. Set high standards, block PRs, call
master broken, for whatever trivial or non-trivial issues you deem
worthy. Turn on `-Wall -Werror`, respect hlint, error out if someone
uses tabs\* or includes trailing whitespace. That's all good.

\* Cue necessary tabs-vs-spaces argument

_However_, when you're releasing your code elsewhere, make the tests
as lenient as possible on optional features. If the code fails to
build: that's a problem. If the code builds, but returns incorrect
runtime results: that's a problem. These should stop build systems
like Stackage from including your package. But stylistic issues, or
newly introduced warnings from the compiler, or myriad other issues,
should not trigger a failure for downstream consumers of your package.
