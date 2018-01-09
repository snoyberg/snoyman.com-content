My
[previous blog post](https://www.snoyman.com/blog/2018/01/drop-conduits-finalizers)
discussed a possible upcoming breaking change to the conduit library:
dropping finalizers. This is one of a number of other breaking changes
I have planned. Another one is switching over from `MonadBaseControl`
to `MonadUnliftIO`, for reasons I've
[discussed](https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets)
[at](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
[length](https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library)
[before](https://www.fpcomplete.com/blog/2017/07/the-rio-monad) and
[spoken](https://www.snoyman.com/reveal/monad-transformer-state)
[about](https://www.youtube.com/watch?v=KZIN9f9rI34) too.

Beyond this change, I have a number of others planned out as well,
some more solidly than others. I've started a document
[describing some of these](https://github.com/snoyberg/codename-karka/#refactor-michaels-existing-libraries-to-match),
and I wanted to bring up one point in this design space for some user
feedback: conduit dependency trees.

## Today

The situation today is that we have a dependency graph that looks
something like the following:

* `resourcet` is at the base of the hierarchy, and defines some
  non-conduit-specific types and functions used throughout the conduit
  ecosystem. It currently depends on a number of packages, like
  `monad-control`, but that number will naturally drop as we move over
  to `MonadUnliftIO` exclusively.
* `conduit` is designed to provide basic conduit functionality with
  fewer dependencies. It _does_ depend on `resourcet`, and packages
  like `monad-control`. But it does not depend on `bytestring`,
  `text`, or `vector`, even though these are almost always wanted with
  conduit. It provides the `Data.Conduit.List` set of combinators,
  which are not the best ones out there.
* `conduit-extra` adds _lots_ of dependencies, including things like
  `attoparsec`, and provides a nicer set of helpers around
  `bytestring` and `text`.
* And finally, at the top of the tree (or our tree for today), we've
  got `conduit-combinators`, which provides the combinators I actually
  recommend people use in the `Data.Conduit.Combinator` module. This
  has lots of dependencies, since it inherits from `conduit-extra` and
  also adds in some extra things like `mwc-random`.

Benefits:

* You can use `resourcet` without touching the conduit ecosystem at all
* You can use `conduit` without pulling in lots of resources
* `Data.Conduit.Combinators` is fully loaded

Downsides:

* The current dependency footprint even at the base is higher than I'd
  like, though that's getting fixed soon regardless.
* The `conduit` package is not super useful on its own due to lack of
  `bytestring`, `text`, and `vector` support.
* To get the functionality you want in either `conduit-extra` or
  `conduit-combinators`, you end up with a _much_ larger dependency
  footprint.

## Plans for the next version

I have a number of different ideas in mind. I'll start off with the
most conservative plan, and mention some variants below.

* As already mentioned, `resourcet` drops a bunch of
  dependencies. Nothing too interesting there.
* `conduit` adds a dependency on `bytestring`, `text`, and `vector` as
  basic libraries everyone should be using anyway. We move over
  `Data.Conduit.Combinators` and provide most of its functionality in
  `conduit` itself, and start recommending against
  `Data.Conduit.List`, `Data.Conduit.Binary`, and `Data.Conduit.Text`.
* `conduit-extra` basically remains as-is
* `conduit-combinators` retains the extra functionality not present in
  the new `conduit`

Benefits:

* The `conduit` package now provides most of the functionality you'll
  want on a day-to-day basis
* The dependency footprint for the `Data.Conduit.Combinators` module
  is much reduced
* We can finally get away from the not-well-named functions in
  `Data.Conduit.List`

There aren't necessarily _downsides_ to this approach, as I think it's
simply better than what we have today already. But I want to list out
the alternatives, which will make clear some things that could be
possibly better still.

* What do we do with the `mono-traversable` package? It's currently a
  dependency of `conduit-combinators`, and the simplest path forward
  for the above is to make `conduit` depend on
  `mono-traversable`. However, this is a slightly heavier dependency
  footprint, requiring adding in `unordered-containers` and
  `vector-algorithms`. Alternatives:
    * Strip down `mono-traversable` to have less deps
    * Redefine parts of `mono-traversable` needed for `conduit` in
      `conduit` itself
    * Going crazy: _really_ move `mono-traversable` into `conduit` and
      swap the dependency tree around
    * My inclination: minimize `mono-traversable`'s dependencies a bit
      more (like dropping the `split` package, and _maybe_
      `vector-algorithms`) and make it a dependency of `conduit`.
* Do we really need `conduit-combinators` as well as `conduit-extra`?
  It's just adding a few extra pieces of functionality over
  `conduit-extra`, and perhaps those should be folded into
  `conduit-extra` itself.
* Some people may not like the heavier dep footprint of `conduit`
  now. Should we split off a `conduit-core` package providing the core
  data types, functions, and operators, and have `conduit` depend on
  that?
* It feels almost silly to have the `ResourceT` data type live in a
  separate package.
    * If we have `conduit-core`, that could be a logical place to put
      it, since it won't have any extra dependencies versus the
      `resourcet` package itself, and then we can turn `resourcet`
      into a backwards compatibility layer.
    * Or it may be logical to place `ResourceT` in the `unliftio-core`
      package, since both concepts help with resource cleanup in monad
      transformers. The former is necessary for continuation-based
      monads, while the latter (`MonadUnliftIO`) works for simpler
      monads.

If people have feedback, I'm happy to hear about it. I've spent an
unfortunate amount of time bouncing around between these different
options, so hopefully writing it all down and hearing some outside
opinions can help move this forward.
