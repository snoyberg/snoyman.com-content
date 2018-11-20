A few days ago I published a post on [the history of
Stackage](https://www.snoyman.com/blog/2018/11/stackage-history-philosophy-future). I
have another proposal coming out soon on formalizing some aspects of
the Commercial Haskell Special Interest Group. Now seems like a good
opportunity to share with you what I think makes Stackage a successful project and how we could improve Haskell resources using similar techniques.

Depending on how you measure it, Stackage is the largest community
project I've ever started. The Github repo has (at time of writing)
567 contributors. For some comparison, Stack has 316 contributors, the
WAI megarepo has 148, and the Yesod megarepo has 188. For those
familiar with the process, contributing to Stackage is a much lower
bar, but as you'll see, that's kind of my point here. Stackage also
has a wonderful team running it called the Stackage Curators,
consisting of [8
people](https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md)
(including myself).

I consider Stackage a success. It has addressed the goals I'd hoped to
achieve (see the aforementioned History of Stackage post for more
information). In this post, I'd like to explore some of the design
choices&mdash;especially on the social side&mdash;that allowed
Stackage to succeed. And then I'd like to see if we can learn some
lessons for community projects in general.

## Clear vision

Stackage starts off with a clear vision: "Stable, vetted Hackage."
That can further be described as:

* Grab upstream Haskell packages
* Try to build them together
* Try to run test suites
* If all of that works, publish a snapshot that end users can use via
  tooling like Stack, Nix, or (with some caveats) cabal-install
* Otherwise, give package authors feedback on what went wrong

This seems straightforward, and makes it easy for people to see if
they care about participating. As a trivial example, we're talking
about Haskell packages here. Someone who exclusively writes Rust knows
that they won't be interested in contributing to this project most
likely.

It also defines what is and is not in scope for Stackage. For example,
an oft requested feature for Stackage is the ability to upload
packages to it. This doesn't fit in with the vision defined above of
grabbing _upstream_ Haskell packages. We do end up getting into some
grey areas (should Stackage patch packages like Debian does? should
Stackage accept packages from locations besides Hackage?). But this
initial vision does a lot to help guide things.

## Easy contribution

As I'm saying a lot recently, I'm a big believer in simplifying the
onboarding experience. Contributing a new package to Stackage is
intentionally very simple: send a pull request adding a few lines to a
YAML file and see if CI passes. We could have demanded more rigorous
criteria, like proof that the package works with the latest Stackage
Nightly. We could have used a different format than YAML. We could
have hosted the repo somewhere besides Github.

However, for our target audience, Github, YAML, and watching a CI
system are all fairly standard. If this was a less technical audience,
I may have instead created a web application with Google-backed
authentication and a webform for entering package names. Make the
contribution and onboarding process as easy as possible for the target
audience.

## Logical requirements

There is one requirement for getting a package into Stackage: it must
build and pass test cases with all of the other packages in the
snapshot. This is a machine checkable requirement, making it easy to
vet whether or not requirements have been met. And the requirements
follow directly from the vision stated above.

I don't think we've yet had someone object to this requirement. If you
don't want to ensure your package is compatible with other packages on
Hackage, that's fine, but clearly Stackage isn't a project you'd be
interested in contributing to. Having the requirement stem directly
from the vision reduces the scope of conflict.

That's not to say that there are no disagreements. We have constant
back-and-forths about how frequently LTS snapshots should be cut, how
aggressively we should prune packages with restrictive upper bounds,
when skipping tests is appropriate, etc. But the scope of these
debates is relatively contained. It also brings us to our next point:

## Public discussion

As this blog post demonstrates, I have a tendency to over-communicate
publicly. But I think this is better than the alternative. We have a
private Stackage Curators channel for discussing day-to-day boring
matters (who's on duty, reviewing blog posts, etc). But all major
decisions these days are shared in an issue tracker, a blog post, a
mailing list, or a Gitter discussion. And usually in more than one of
those!

Furthermore, we try hard to document the decisions we've made in the
past. Both the
[`MAINTAINERS.md`](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md)
and
[`CURATORS.md`](https://github.com/commercialhaskell/stackage/blob/master/CURATORS.md)
say quite a bit about how Stackage is run. We've tried to stress-test
these documents by having new members of the curator team onboard
themselves with the documentation, and then asking questions on what's
unclear. If information is missing, we fill it into the
documents. Ideally, little to no information is retained exclusively
in someone's head.

I wouldn't say we've been perfect here. In fact, I've been the biggest
culprit of problems here. When Stackage was a one-man show, I _did_
have a lot of knowledge trapped in my head undocumented. I _did_
implicitly make decisions by myself (many times without even realizing
it). The rest of the curator team has been a very good influence on
me, and has encouraged more structure in how we document and discuss
our work.

## Positive benefits for participation

If someone asked me "why should I participate in Stackage?" I could
give some clear benefits:

* Get notified if your packages no longer compile with their dependencies
* Get your test suites run in a different environment for more thorough testing
* Make it easy to access your packages from Stack and Nix

If those benefits are appealing, and outweigh the costs of
contributing to Stackage, then great! And if not, no hard
feelings. Stackage is fully opt-in, and therefore there's only
positive pressure to be a part of it, no negative backlash for failing
to comply.

## Reasonably decent automation

Over the years, the stackage-curator tool has evolved quite a
bit. Personally, I think the process of curating Stackage is
relatively easy. I hope my fellow curators feel the same way. But with
2,000 packages being built regularly, it would be all but impossible
to maintain Stackage without decent tooling. With that said, let's get
to improvements.

## Improvements

Stackage can and should improve. I believe the two biggest areas for improvement currently are:

* __Tooling__ stackage-curator is good, but it's showing its
  age. We've already got [some plans for
  improvements](https://github.com/commercialhaskell/stack/issues/4217),
  including possibly using Nix for doing the building (to both
  simplify our tooling, and get better testing of the `stack2nix`
  pipeline).
* __Communications__ As I mentioned, the curator team has been a great
  influence on better communication. We're hoping to continue
  that. Some of the tooling improvements will hopefully make it easier
  to communicate why packages have been held back in LTS minor bumps,
  for example.

I'm sure there are other improvements to be made as well, these are
just the highest two on my personal list.

## Lessons for other projects

As I mentioned, I'm going to be following up soon with a proposal
related to Commercial Haskell. I'm hoping the points above can
influence other projects towards making good decisions towards
success. For example, in the case of Commercial Haskell, I don't think
we've done enough to clarify the vision of the project.

We can also apply some of these lessons to other Haskell community
projects. For example:

* It seems like Hackage has a clear vision: a central package
  repository for all open source Haskell code which uses the Cabal
  build system. However, a secondary goal&mdash;complying with the PVP
  in order to allow for dependency solving&mdash;has been a highly
  contentious issue. I've already [requested clarification of this
  point](https://www.snoyman.com/blog/2018/02/haskell-ecosystem-requests). In
  my opinion, if there was a clearly stated vision for Hackage in
  _either_ direction, many of our problems would be solved.
    * If the vision is "central package repository," then it follows
      naturally that non-PVP-compliant code should still be encouraged
      to be submitted to Hackage, since that's in keeping with the
      vision statement.
    * If, by contrast, PVP compliance for dependency solving is the
      goal, then it makes sense that people with different goals would
      upload their Haskell packages elsewhere.
* Haskell Platform (the original, full version) solved multiple
  different goals at the same time:
    * An easy method for installing the Haskell toolchain
    * An opinionated set of "batteries included" packages
    * A committee process for vetting libraries for high quality

  I don't see a problem with _any_ of these goals. However, putting
  them together created problems. For example, bundling of the
  "batteries included" packages with HP led to a situation where it
  was (with old versions of cabal-install) difficult to upgrade
  packages, leading to in some cases of highly-security-vulnerable
  packages being shipped for years.

  Ultimately, the Haskell Platform Core installer addressed a large
  part of this issue, since it minimized the vision of the project (an
  installer provided the most common tooling, including _both_
  cabal-install and Stack)

* What's the purpose of the haskell.org homepage? This has been a
  highly fought over issue, but defining a vision would have helped
  significantly. What is the purpose?

    * Convince hobbyists to try it out
    * Provide a homepage for professors to send their students to get
      installation instructions
    * Be a hub for Haskell community information and discussion
    * Document all common methods of using Haskell

  Stating what the vision is, having public discussion about how to
  achieve it, and finding methods to address alternative goals, could
  greatly help with community tensions over the site.

* Within Stack&mdash;as I mentioned in my previous post&mdash;I see some possibilities for decoupling. For example, we could separate off the GHC installation code from everything else. This mostly falls into the same category: defining a clear vision (opinionated build tool) and letting the less opinionated piece (method of installing GHC) stand on its own.

* haskell-lang.org was _not_ started correctly. The vision was wrong. It should have been something like "Haskell for people who want a highly opinionated set of guidelines." The naming should have reflected that. Hopefully that's something that can be rectified in the near future.
