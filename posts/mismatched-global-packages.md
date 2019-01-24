This blog post covers an issue with how packages are shipped with GHC, and an edge case of how this can negatively interact with Stack. The overall point about the package contents mismatch will apply more broadly, but I'm mostly focused in this post on how to better handle this in Stack.

## Intro

GHC ships with a few _wired in_ packages which cannot be reinstalled. Examples are `ghc`, `base`, and `template-haskell`. The situation with these for all build tools is simple: the GHC version determines the package version.

There are also packages which are not shipped with GHC at all. All build tools get to determine whatever version of these packages they want, assuming compatibility with the GHC version and the packages that are wired in with it.

There's an in-between category: packages like `process` and `transformers` are shipped with GHC, but can also be easily reinstalled from Hackage. Some tools may decide to eagerly recompile the latest version of them. Stackage takes a different approach: in order to avoid invalidating the `ghc` package, it avoids recompiling any of these packages, and sticks by default with the _global version_. Stack is designed to work with that.

## When recompilation happens

Suppose you have the following `stack.yaml` file:

```yaml
resolver: ghc-8.6.3
packages:
- .
```

And suppose your local package depends on the `unix` package. On non-Windows systems, `unix` ships with GHC. Running `stack build` will therefore result in only building the local package, not `unix`, since `unix` is in the global package database.

Now let's suppose that, instead of using a `ghc-8.6.3` resolver, you use an LTS Haskell resolver:

```yaml
resolver: lts-13.4
packages:
- .
```

Same thing: LTS Haskell snapshots never specify versions of global packages. Instead, they are implicitly inherited from the GHC global package database. In a few specific cases (like the `stack init` command), we want to avoid requiring that GHC is already installed, so we instead use a [global hints](https://github.com/commercialhaskell/stackage-content/blob/master/stack/global-hints.yaml) file to specify the versions of packages which ship with GHC.

The `unix` package depends on the `time` package. Let's suppose your `stack.yaml` file instead said:

```yaml
resolver: lts-13.4
packages:
- .
extra-deps:
- time-1.9.2
```

And suppose your local package depends on both `unix` and `time`. If you run `stack build`, Stack has three basic options:

1. Only recompile `time`, but keep the original `unix`. That's a bad idea: you'll now have two different versions of the datatypes in the `time` package floating around. People may remember extremely confusing error messages about "cannot match `ByteString` with `ByteString`" or similar. It comes from this kind of a build plan.
2. Fail to compile at all. The `unix` in the global database is "invalidated" by the presence of a newly compiled `time`, and your package depends on `unix`. Just give up.
3. Automatically add an `extra-dep` version of `unix` to the build plan, and recompile both `time` and `unix`.

(1) is a really bad idea, and (2) is pretty annoying. So today, Stack follows (3), and automatically/implicitly adds the invalidated global packages to the build plan.

## Where do global packages come from?

Look at the following simplified output of a command describing the global `process` package shipped with GHC 8.6.3:

```
$ stack --resolver ghc-8.6.3 exec -- ghc-pkg describe process
name: process
version: 1.6.3.0
...
depends:
    base-4.12.0.0 deepseq-1.4.4.0 directory-1.3.3.0 filepath-1.4.2.1
    unix-2.7.2.2
```

Important bits:

* We're using `process-1.6.3.0`
* We're depending on `base-4.12.0.0`

Now check out the `process.cabal` file for `process-1.6.3.0` [on Hackage](https://hackage.haskell.org/package/process-1.6.3.0/revision/0.cabal) and [on Github](https://github.com/haskell/process/blob/v1.6.3.0/process.cabal#L77). You'll notice the following bound:

```
    build-depends: base      >= 4.4 && < 4.12,
```

These pieces of information are contradictory: the cabal file says `base < 4.12`, but GHC ships with this version of `process` and `base-4.12.0.0`! How is this possible?

Well, it turns out, when GHC says it has `process-1.6.3.0`, it's _not_ code that's downloaded from Hackage. Instead, it's established via a submodule. Specifically, you can [see on Github](https://github.com/ghc/ghc/tree/ghc-8.6.3-release/libraries) that GHC 8.6.3 is using `process` at Git commit [36a3ad5](https://github.com/haskell/process/tree/36a3ad577e31e8c3336c7464b252fc2c9b01a20c). By contrast, the `v1.6.3.0` tag points to Git commit [7c0b5814](https://github.com/haskell/process/commit/7c0b58141290b50a338bf391adc0a8c43513165b).

This isn't a recent revelation, and not only my revelation. I peripherally knew about this, but I've heard of three different people discovering this problem in the past month. I'm writing this blog post to point out how it negatively affects Stack, and what to do about it.

## How it breaks Stack

`process` depends on the `directory` package. Let's use an extra-dep to provide a new version of `directory`, and then try to build:

```
$ cat stack.yaml 
resolver: ghc-8.6.3
packages: []
extra-deps:
- directory-1.3.3.1
$ stack build process

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for process-1.6.3.0:
    base-4.12.0.0 from stack configuration does not match >=4.4 && <4.12  (latest matching version is 4.11.1.0)
needed since process is a build target.
```

Well that's darn confusing. I changed a version of `directory`, and suddenly `process` has a bounds error?!? This makes perfect sense given the above (though it's certainly unsatisfying):

* GHC ships with a _different version_ of `process-1.6.3.0`
* `process` needs to be recompiled when we change the `directory` package
* Stack assumes that it can simply use `process-1.6.3.0` to replace the global `process` package
* However, the `process` on Hackage differs slightly from the one shipped with GHC

This is a relatively simple problem which results in an annoying bounds error, and can be easily fixed by adding a new `process` extra-dep. However, other problems are much more severe:

* What if GHC is compiling one of these packages with a special cabal flag? We won't be able to reproduce that. In fact, there are comments in the Stack code base about this already.
* What if the code on Hackage is _fundamentally_ different from what's shipped with GHC? We could get silent but significant behavior changes.

## Solution: make no assumptions

The core problem here is that Stack assumes `process-1.6.3.0` provided by GHC is the same as what's on Hackage. That assumption plays itself out in our selection of option (3) above. I propose: get rid of the assumption! Instead, move to option (2): if we replace a global package, prune all global packages that depend on it, and require adding those as explicit `extra-dep` entries.

This will break some existing build plans, but I'm proposing this change for Stack 2.0, which is already allowing some slight breakage along those lines. This will add some annoyance to users, but with the result of more clarity on what's happening in a build plan. And perhaps we can even make an error message for these cases which points right at this post!
