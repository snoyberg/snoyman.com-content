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

__UPDATE__ Someone pointed out that this "unwanted work" description
was pretty vague. To clarify, here's an example situation:

1. Package author Alice uploads package `foo`, and depends on `bar >=
   2.3` (with no upper bounds). The latest version of `bar` on Hackage
   is 2.3.
2. Hackage Trustee Bob adds an upper so that, now, `foo` depends on
   `bar >= 2.3 && < 2.4`
3. Package author Charlie uploads `bar-2.4` to Hackage.
4. Alice checks on her Git repo and sees that the current `foo` code
   is compatible with `bar-2.4`, so does nothing.
5. Meanwhile, Stackage discovers the upper bounds and files a bug
   report for Alice to remove the upper bound (that she's not aware
   of).
6. Alice needs to log in to Hackage and remove the upper bound (or at
   least relax it).
7. Alternatively, with `no-revisions` in place, Alice could initially
   put `foo` in the `no-revisions` list, and then Bob's changes would
   be ignored completely by Stackage.

This is an opt-in field, so people who want the current behavior need
not do anything. This change will transparently work for Stack, since
it will simply respect the hash of the .cabal file. And since there
may be some negative ramifications of this change I haven't
considered, I'm calling this feature experimental and asking for
feedback if this causing anyone some issues.

Hopefully this change will let people who are using the Stack and
Stackage toolchain work with less interference, with less friction
occurring with Hackage Trustees making version bounds modifications.
