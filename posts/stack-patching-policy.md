This blog post is about a potential policy decision affecting the
maintenance of the Stack code base itself. It will affect contributors
to the project, and those building Stack for other purposes (such as
maintainers of Linux distro packages). It will only indirectly affect
end users, as hopefully is made clear in the discussion below.

[**Github issue for official discussion**](https://github.com/commercialhaskell/stack/issues/3866)

## Today

Until now, every version of Stack that has been released (or even
merged to `master`, unless I'm mistaken) has exclusively used versions
of dependencies available on Hackage. It has not used the `extra-dep`
archive or Git repo feature, or submodules to include alternative
versions of source code. This means that, for the most part, you get
the same Stack whether you get an official download, run `stack build`
inside the source tree, use `stack build` using a Stackage snapshot,
or run `cabal install stack`.

Now, as it happens, this isn't _completely_ true either. The official
Stack binaries pin the dependencies to exact versions which have been
tested together, via the `stack.yaml` file. This means that the latter
two approaches of getting Stack binaries may have different behavior,
due to the snapshot or the dependency solver choosing different
versions. Some distros have
[already run into bugs](https://github.com/commercialhaskell/stack/issues/3848)
because of this.

To pull all of that back in: the official way to get Stack today will
guarantee a specific set of dependencies which have gone through the
full Stack integration test suite. Some alternative methods may not
provide the same level of guarantees. But with a bit of effort, you
can force Stack or cabal-install to build exactly the same thing the
official binaries provide.

## The new problem

One issue that pops up is: what do we do in a situation where an
upstream package has a bug, and either cannot (within the timeframe
desired) or will not release a new version with a fix? The concrete
example that pops up is
[hackage-security pull request #203](https://github.com/haskell/hackage-security/pull/203)
(addressing
[issue #187](https://github.com/haskell/hackage-security/issues/187)),
though the specific details aren't too important for the discussion
here. The discussion here is about the general rule: _what should
Stack do in this case_?

## Four options

Others may be more creative than me, but I can see four different
options to respond in a situation like this:

1. Continue using the officially released upstream version of
   hackage-security, bugs and all
2. Fork hackage-security on Hackage, and depend on the fork
3. Inline the code from hackage-security into Stack itself, and drop
   the explicit dependency on hackage-security
4. Include hackage-security via an `extra-dep` pointing at a Git
   commit. Our official builds will use the patched version of
   hackage-security, and anyone building from Hackage will end up with
   the unpatched version

Option (1) is the status quo: we cannot fix this bug until upstream
fixes it. This is a disappointing outcome for users, as we know how to
fix the bug, and can imminently do so, but users will continue to
suffer regardless. However, it makes maintenance of Stack relatively
easy, and has no impact on packagers.

Options (2) and (3) are relatively similar: you end up with a forked
version of the codebase feeding into Stack, but all of the code
necessary is still available from Hackage. Packagers, and people
building with a command like `cabal install stack`, will still be able
to get the right version of the executable, assuming they pin their
dependencies the same way we do (as mentioned above).

Option (4) is a more radical departure. It means that `cabal install
stack`, without quite a bit of extra work, will _not_ result in the
same executable. You can argue that, given the assumed lack of pinning
of dependency versions, this isn't too terribly different from the
status quo. And with the patch I've written for hackage-security now,
that's basically true. However, it's theoretically possible that, in
the future, we could have a patch that changes the API, and makes it
impossible to build Stack against the Hackage version of a package. So
let's break up option 4 into two subchoices:

* Option 4a: we can use an extra-dep, but we must ensure that the
  Stack codebase continues to build against the Hackage version of the
  package, even if it's missing a bug fix, performance enhancement, or
  whatever else we wrote
* Option 4b: free-for-all: use whatever extra-deps we want, and state
  that there is no support for building from Hackage alone.

## My recommendation

I lean towards option 4a. I don't want to upload forks to Hackage
(option (2)); it's a confusing situation for users, and may be seen as
an aggressive move (which is certainly not the intent here). Option
(3) could work, but makes it more painful than it should be to work on
the Stack codebase. I'd rather not subject contributors (or myself!)
to that.

Option 4b is IMO a step too far: we'd be uploading something to
Hackage which we know for a fact could never be built there. At that
point, there's not really any reason for uploading to Hackage. And
option 1 (we cannot fix bugs) is just too limiting.

The biggest impact I can see is how others will end up packaging
Stack. But frankly, this is already a situation that deserves an
official discussion. There have certainly been plenty of cases in the
past where users tripped on bugs that didn't exist in the official
Stack releases, and the Stack team needed to spend inordinate time
tracing this back to a bad build. So if nothing else, hopefully this
post will spawn some discussion of correct packaging behavior.

## Official discussion

As mentioned above, I've created a Github issue for an official
discussion of this topic:
[issue #3866](https://github.com/commercialhaskell/stack/issues/3866). Other
discussions (Disqus below, mailing list, etc) are welcome, but may not
receive the full attention of the Stack team.
