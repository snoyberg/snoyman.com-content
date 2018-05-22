A question popped up the other day on Stack Overflow, and I thought I'd write
up in a bit more detail how to solve it. There are some packages that we'd like
to install, but which are not in Stackage snapshots. Trying to build them may
end up with an error message like this:

```
$ stack install idris

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for idris-1.2.0:
    ansi-terminal-0.8.0.4 from stack configuration does not match <0.8  (latest matching version is 0.7.1.1)
needed since idris is a build target.

Some different approaches to resolving this:

  * Set 'allow-newer: true' to ignore all version constraints and build anyway.

  * Consider trying 'stack solver', which uses the cabal-install solver to attempt to find some working build
    configuration. This can be convenient when dealing with many complicated constraint errors, but results may be
    unpredictable.

  * Recommended action: try adding the following to your extra-deps in /home/michael/.stack/global-project/stack.yaml:

- ansi-terminal-0.7.1.1

Plan construction failed.
```

(Note: depending on when you run these commands, view the pages below, and
local settings, you may get different output.)

What this is saying is that Stack is currently configured to use a snapshot
which includes `ansi-terminal-0.8.0.4`. However, `idris-1.2.0` has an upper
bound on `ansi-terminal` that prevents that usage. There are probably two
solutions to this problem that you'd think of (especially since the error
message mentions them):

1. Ignore the dependency bounds and hope for the best with `--allow-newer`
2. Use a dependency solver (via `cabal-install`) to try to come up with a
   complete new build plan

Both of these approaches run the risk of coming up with a build plan that
doesn't work, resulting in wasted time ending in a failed compilation. However,
in some cases, there's another option in some cases. If I look at the [package
page for Idris on Stackage](https://www.stackage.org/package/idris), I see the
following:

<img src="https://i.imgur.com/FRqRlOc.png" alt="Package page for Idris" style="max-width: 100%">

It turns out that Idris 1.2.0 already appeared in a Stackage snapshot. At the
very least, it was in LTS Haskell 10.10 and Stackage Nightly 2018-03-10. We can
[follow that link at the
bottom](https://www.stackage.org/package/idris/snapshots) to get a complete
list of snapshots Idris appears in.

With this in mind, it's easy to build Idris:

```
$ stack install --resolver lts-10.10 idris
```

This gives you a way to tell Stack about a known good build plan and avoid
spinning your wheels. This approach works best for packages providing
executables you want to use, as opposed to libraries you want to build on. For
the latter, this approach would likely end up pointing you at multiple
conflicting snapshots. Overall, the best bet there is to get the missing
packages [added to
Stackage](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md).

You may ask: couldn't Stack automatically do this for me? Well, it can't
_guess_ what you want in this case. It would be ambiguous if you're trying to
build a package with your currently selected snapshot so that it's available to
your code, for example, versus just creating an executable. But it would also
be possible to add a flag like `--select-latest-snapshot` or similar to
automatically choose the snapshot with the best chance of building the package.
`stack init` already does something like this.
