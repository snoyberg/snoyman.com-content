It's
[the oldest open issue on the Stackage repo](https://github.com/fpco/stackage/issues/416),
and a topic I've discussed more times than I can remember over the
years. Hackage enforces that _package names_ are unique (so that no
one else can claim the name `conduit`, for instance), but does nothing
to ensure unique _module names_ (so someone else could write a package
named `my-conduit` with a module named `Data.Conduit`).

For the record, I think Hackage's position here is not only a good
one, but the only logical one it could have made. I'm not even hinting
at wanting to change that. Please don't read this blog post in that
way at all.

Usually, conflicting module names do not negatively affect us, at
least when working on project code with a proper `.cabal` file. In my
made-up example above, I would explicitly state that I depend on
`conduit` and not list `my-conduit`, and when my code imports
`Data.Conduit`, Stack+Cabal+GHC can all work together to ensure that
the correct module is used.

__EDIT__ Since I've already written some of the code for stackage-curator to
detect this, I generated a [list of all conflicting module
names](https://gist.github.com/snoyberg/c5044f390d22200fcee37c894a853719) to
give an idea of what we're looking at.

## The problem

(If you're already convinced that conflicting module names are a
problem, you may want to skip straight to "the solution." This section
is fairly long and detailed.)

Unfortunately, there are still some downsides to having the same
module name appear in different packages:

1.  __Documentation__ Suppose I'm reading a tutorial that includes the
    line `import Control.Monad.Reader`. I look at the
    [Stackage doc list by module](https://www.stackage.org/lts/docs)
    and discover:

    ![monads-tf and mtl](http://i.imgur.com/FkeC6ak.png)

    If I'm not familiar with the Haskell ecosystem, I'm unlikely to
    know that `mtl` is far
    [more](https://www.stackage.org/package/mtl#reverse-dependencies)
    [popular](https://www.stackage.org/package/monads-tf#reverse-dependencies)
    than `monads-tf` and choose the latter.

2.  __runghc/ghci__ We're not always working on project
    code. Sometimes we're just writing a script. Sometimes we're
    playing with an idea in GHCi. What if I `import
    System.FilePath.Glob` in a GHCi prompt when I have both the
    `filemanip` and `Glob` packages installed?

3.  __doctests__ Similar to the previous point: even when you run
    doctests from inside the context of a project, they don't
    typically know which packages can be used, and conflicting module
    names can
    [cause the tests to fail](https://github.com/yesodweb/wai/issues/579). What's
    especially bad about this is that an unrelated action (like
    running `stack build async-dejafu`) can suddenly make your tests
    start to fail when they previously succeeded.

4.  __Custom Setup.hs__ Suppose you're writing a cabal package that
    uses a custom `Setup.hs` file and imports some additional
    modules. To pick a concrete example that just happened: the
    `executable-hash` package has a `Setup.hs` file which -
    indirectly - imports `Crypto.Hash.SHA1`. And there's an explicit
    dependency on `cryptohash` in the `.cabal` file, which one may
    naively infer means we're safe. However, when `uuid-1.3.13` moved
    from cryptonite to a few other packages (including
    cryptohash-sha1), building `executable-hash` when `uuid` was
    already installed became a build error. And like the previous
    point, this is essentially a non-deterministic race condition.

    Since I was a backup maintainer for `executable-hash`, I
    implemented two fixes:
    [adding an explicit `PackageImport`](https://github.com/fpco/executable-hash/commit/91ee923513b6464a49f02fdc0738202e7b4a907a)
    and
    [using the new custom-setup feature in Cabal-1.24](https://github.com/fpco/executable-hash/commit/e93af5ed6e1e46efc876ab008bd48574c761780c). While
    custom-setup is definitely the way to go with this, and it's a
    great addition to Cabal, not everyone is using the newest version
    of Cabal, Stack is only just now adding support for this, and not
    all packages will update to support this immediately.

5.  __Better tooling__ It would be great if tooling could
    automatically determine which packages to install based on the
    imports list, to avoid the need for a lot of manual and redundant
    statements of dependencies. We're considering
    [doing this in the upcoming `stack script` command](https://github.com/commercialhaskell/stack/issues/2805#issuecomment-263075097). But
    how will Stack know which `Control.Monad.Reader` to use?

## The solution

While we know that we can't have fully unique module names without a
lot of buy-in from package authors, we can get pretty close, with
_canonical_ locations for a module. We've already implemented this to
some extent in Stackage to resolve problem (3) listed above. We now
have the ability to list some packages as _hidden_ in a Stackage
snapshot. This means that, after installing the package, the Stackage
build system will _hide_ the package, so that its modules won't be
available for import. By adding `async-dejafu` to the hidden list, the
warp doctest suite no longer has the ambiguity issue when running.

After dealing with the cryptohash-sha1 fallout earlier this week, I
realized that this solution can generalize to solve a large swath of
the problems described above. Here's how I see it working:

* We introduce a new constraint in the Stackage build process: every
  module name must be present in only one _exposed_ (that is,
  non-hidden) package.
* When `stack build` registers a package, it automatically hides it if
  the snapshot lists it as hidden.
* On the stackage.org module list, modules from a hidden package are
  explicitly marked as hidden (or, if we want to be more extreme, we
  just hide them entirely).
* With the upcoming `stack script` command, when finding a package for
  a given imported module, we only pay attention to non-hidden
  modules.

This doesn't fully solve the problems above. For example, if a user
just Googles `Control.Monad.Reader`, they'll still possibly get
confusing documentation. But I think this is a huge step in the right
direction.
