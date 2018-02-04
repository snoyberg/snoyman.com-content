At the end of last week, I made a number of breaking releases of
libraries. The API impact of these changes was relatively minor, so
most code should continue to work with little to no modification. I'm
going to call out the major motivating changes for these releases
below. If I leave out a package from explanations, assume the reason
is just "upstream breaking changes caused breaking changes here." And
of course check the relevant ChangeLogs for more details.

For completeness, the list of packages I released at the end of last
week is:

- conduit-1.3.0
- conduit-extra-1.3.0
- network-conduit-tls-1.3.0
- resourcet-1.2.0
- xml-conduit-1.8.0
- html-conduit-1.3.0
- xml-hamlet-0.5.0
- persistent-2.8.0
- persistent-mongoDB-2.8.0
- persistent-mysql-2.8.0
- persistent-postgresql-2.8.0
- persistent-sqlite-2.8.0
- persistent-template-2.5.3.1
- persistent-test-2.0.0.3
- conduit-combinators-1.3.0
- yesod-1.6.0
- yesod-auth-1.6.0
- yesod-auth-oauth-1.6.0
- yesod-bin-1.6.0
- yesod-core-1.6.0
- yesod-eventsource-1.6.0
- yesod-form-1.6.0
- yesod-newsfeed-1.6.1.0
- yesod-persistent-1.6.0
- yesod-sitemap-1.6.0
- yesod-static-1.6.0
- yesod-test-1.6.0
- yesod-websockets-0.3.0
- classy-prelude-1.4.0
- classy-prelude-conduit-1.4.0
- classy-prelude-yesod-1.4.0
- mutable-containers-0.3.4

## Switching to MonadUnliftIO

The primary instigator for this set of releases was moving my
libraries over from `MonadBaseControl` and `MonadCatch`/`MonadMask`
(from the `monad-control` and `exceptions` packages, respectively)
over to `MonadUnliftIO`. I
[gave a talk recently](https://www.youtube.com/watch?v=KZIN9f9rI34)
([slides](https://www.snoyman.com/reveal/monad-transformer-state)) at
LambdaWorld about this topic, and have
[blogged](https://www.fpcomplete.com/blog/2017/07/announcing-new-unliftio-library)
[at](https://www.fpcomplete.com/blog/2017/07/the-rio-monad)
[length](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
[as well](https://www.fpcomplete.com/blog/2017/06/tale-of-two-brackets).
Therefore, I'm not going to get into the arguments here of why I think
`MonadUnliftIO` is a better solution to this class of problems.

Unless I missed something, this change dropped direct dependency on
the `monad-control`, `lifted-base`, and `lifted-async` packages
throughout all of the packages listed above. The dependency on the
`exceptions` package remains, but only for using the `MonadThrow`
typeclass, not the `MonadCatch` and `MonadMask` typeclasses. (This
does leave open a question of whether we should still define valid
instances of `MonadCatch` and `MonadMask`, see
[rio issue #38](https://github.com/commercialhaskell/rio/issues/38).)

__User impact__: You may need to switch some usages of the
`lifted-base` package to `unliftio` or similar, and update some type
signatures. It's possible that if you're using a monad transformer
stack which is not an instance of `MonadUnliftIO` that you'll face
compilation issues.

## Safer runResourceT

In previous versions of the `resourcet` package, if you register a
cleanup action which throws an exception itself, the exception would
be swallowed. In this new release, any exceptions thrown during
cleanup will be rethrown by `runResourceT`.

## conduit cleanups

There were some big-ish changes to conduit:

* Drop finalizers from the library,
  [as discussed previously](https://www.snoyman.com/blog/2018/01/drop-conduits-finalizers). This
  resulted in the removal of the `yieldOr` and `addCleanup` functions,
  and the replacement of the `ResumableSource` and `ResumableConduit`
  types with the `SealedConduitT` type.
* Deprecated the old type synonyms and operators from the
  library. This has been
  [planned for a long time](https://www.snoyman.com/blog/2016/09/proposed-conduit-reskin).
* Moved the `Conduit` and `Data.Conduit.Combinators` modules from
  `conduit-combinators` into `conduit` itself. This increases the
  dependency footprint of conduit itself, but makes it a fully loaded
  streaming data library. `conduit-combinators` is now an empty
  library.

## Yesod: no more transformers!

The changes mentioned in
[my last yesodweb.com blog post](https://www.yesodweb.com/blog/2018/01/upcoming-yesod-breaking-changes)
have been carried out. The biggest impact of that is replacing
`HandlerT` and `WidgetT` (as transformers over `IO`) with `HandlerFor`
and `WidgetFor`, as concrete monads parameterized by the site data
type. Thanks to backwards compat `HandlerT` and `WidgetT` type
synonyms, and the Template Haskell-generated `Handler` and `Widget`
synonyms being updated automatically, hopefully most users will feel
almost no impact from this. (Authors of subsites, however, will likely
have a more significant amount of work to do.)

## That's it?

Yeah, this post turned out much smaller than I expected. There are
likely breakages that I've forgotten about and which should be called
out. I'll ask that if anyone notices particular breakages they needed
to work around, to please either include a note below or send a PR to
this blog post (link above) adding information on the change.
