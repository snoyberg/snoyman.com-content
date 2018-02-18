Last month, I
[clarified some parts of the SLURP proposal](https://www.snoyman.com/blog/2018/01/slurp). I'm
intentionally _not_ getting into the SLURP proposal itself here, if
you missed that episode, don't worry about it. One of the outcomes of
that blog post was that I shared some of the requests I had made in
private that ultimately led to the SLURP proposal.

A single comment in a mega-thread on Github is hardly a good place to
write down these requests, however, and it seems like there's no
progress on them. I'm going to instead put down these ideas here, with
a bit more explanation, and a few more ideas that have popped up since
then.

(If you really want to, feel free to see the context of my
[original comment](https://github.com/haskell/ecosystem-proposals/pull/4#issuecomment-360044280).)

These points should be made in some kind of more official forum, but:

1. I'm honestly not sure where that forum is
2. I don't believe the official forums we typically use for
   discussions of community infrastructure are nearly visible enough
   to most community members

So I'll start the conversation here, and later we can move it to the
right place.

## PVP adherence is optional

I would like to see some kind of statement on Hackage that says
something like, "PVP adherence is recommended, but not required. You
are free to upload a package even if it does not conform to the PVP."
Which I realize is in fact _exactly_ what the current policy is, but
in many discussions, this was unclear to people. And have a clear
sentence to be quoted when online discussions get heated would be
useful. Without something like this, I believe that we will continue
having regular online flamewars about the PVP, which is the biggest
thing I've been trying to get to stop over the past few years.

## Hackage Trustee guidelines

Going along with this, I would like to request a change to the Hackage Trustee
guidelines (or whatever the appropriate term is), namely that it is
not appropriate to PVP police on social media. Sending PRs and opening
issues: totally acceptable. Emails to authors: totally acceptable. If
an author requests that these stop: they must stop. Publicly
criticizing an author for not following the PVP: unacceptable. I _do_
realize that enforcing a policy on how people behave personally is
difficult. But I'd be happy to see the change _even if it wasn't
easily enforceable_.

## Downstream projects

Private discussions tried to achieve some kind of technical policy
which would avoid breakage to Stackage and Stack. It seems like those
private discussions did not reach any conclusion. However, regardless
of any technical policy that is put in place, I would request simple
goal be stated:

__GHC, Hackage, and Cabal will strive to meet the needs of commonly
used downstream projects, including but not limited to Stackage,
Stack, and Nix.__

I'm not asking for any demands of compatibility or testing, simply a
stated policy that "it works with cabal-install, that's all that
matters" is not a sufficient response.

## Maintainer guidelines

There have been a number of issues and pull requests recently where
contributors to some infrastructure projects have been discouraged by
the unclear process for getting their changes included upstream. See,
as examples:

* <https://github.com/haskell/cabal/pull/4696>
* <https://github.com/haskell/hackage-security/pull/203>

More generally, there is an ongoing culture in some places of
goals/agendas/plans being made privately and not shared, which leads
to an inability of people outside of an inner circle to
contribute. See, for example:

* <https://np.reddit.com/r/haskell/comments/7tykqi/should_stackage_ignore_version_bounds/dtgn1eb/>

I would like to recommend some maintainer guidelines be put in place
for any core Haskell packages and projects. (What constitutes "core"
could definitely be up for debate as well.) I'd like to see some rules
like:

* Plans for significant changes must start as an issue in an issue
  tracker (see
  [Gabriel's golden rule](https://twitter.com/GabrielG439/status/963659683557933057))
* Plans for major changes should have a mention in a more public forum
  than an issue tracker. As a concrete example: the newly added `^>=`
  operator has significant impacts on how downstream projects like
  Stackage interact with dependency bounds, but no public comment
  period was granted to provide input before the 2.0 release. (And
  even post release, as referenced above, the full plan has not been
  revealed.)
* Pull requests which are rejected are given a reason for being
  rejected (this includes simple refusal to merge). See, for example,
  [hackage-security #206](https://github.com/haskell/hackage-security/pull/206#issuecomment-366153260).

There are likely many other guidelines we could come up with, some
more onerous than others. I encourage others to recommend other
ideas too. One possible source of inspiration for this could be the
[maintainer communication advice](https://github.com/commercialhaskell/commercialhaskell/blob/master/guide/maintainer-communication.md)
I wrote up a few years ago.
