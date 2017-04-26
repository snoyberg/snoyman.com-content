I'm announcing a new, experimental field in the
[`build-constraints.yaml` file for Stackage](https://github.com/fpco/stackage/blob/1be82f516d5c3c4f276c8119bbae018034a42b25/build-constraints.yaml#L3979). For
those not familiar with Hackage revisions, let me give a quick rundown
of how things work:

* When you upload a package to Hackage, your tarball includes a .cabal
  file
* That .cabal file gets included in the index tarball containing all
  .cabal files
* From the Hackage website, package maintainers and Hackage Trustees
  are able to edit some metadata about a package, like its dependency
  bounds
* When such an edit takes place, a new .cabal file is added at the end
  of the index tarball with the updated content
* It is the responsibility of tooling (like cabal, Stack, and the
  Stackage build tools) to&mdash;when extracting a package's
  tarball&mdash;replace the original .cabal file with the correct
  version, usually the newest version available

When a Stackage build runs, until now it would always take the most
recent revision of a .cabal file and use that for bounds checking (and
other activities). Then, when creating a snapshot, it would include a
hash of the revision of the .cabal file it used. That hash is in turn
used by Stack to ensure that&mdash;when building a package from a
snapshot&mdash;it uses the same revision for reproducibility.

(Sound complicated? It kind of is.)

OK, all that said: what's the problem? Well, there are some
disagreements in certain cases about whether a revision to a package's
.cabal file should have occurred. An unwanted revision can create
undesired work for the package author. After this situation arose a
few times, I discussed with some involved parties, and came up with
the `no-revisions` field. Its purpose is really simple:

> When the Stackage build tool is choosing which .cabal file revision
> to use, if a package is present in the `no-revisions` list, then the
> original revision is used. Otherwise, the newest revision is used.

This is an opt-in field, so people who want the current behavior need
not do anything. This change will transparently work for Stack, since
it will simply respect the hash of the .cabal file. And since there
may be some negative ramifications of this change I haven't
considered, I'm calling this feature experimental and asking for
feedback if this causing anyone some issues.

Hopefully this change will let people who are using the Stack and
Stackage toolchain work with less interference, with less friction
occurring with Hackage Trustees making version bounds modifications.
