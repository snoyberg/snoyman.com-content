Both the Stackage and Stack projects originated many years ago. At the
time they started, they had specific goals geared at solving problems
at the time. Some of those problems remain, some have changed. Also,
as the projects have continued, some goals have morphed as well.

I recently received some questions from newer members of the Haskell
community who were uncertain of some design points in Stackage. As I'm
wont to do, instead of giving a targeted answer to those questions,
I'm going to take the opportunity to provide a more in depth review of
the history of these projects, the philosophy governing how they work,
and some (highly opinionated) thoughts on what the future may hold for
them.

## Further reading

To start off with, let's give some historical links. I've typically
been pretty vocal about what I'm working on, so there are quite a few
posts available. I'm also assuming people aren't going to go through
and read all these posts, so I'll be covering a simplified view of the
history next.

* [Avoid cabal hell: find nirvana](https://www.yesodweb.com/blog/2012/03/cabal-nirvana)
* [Announcing the Yesod Platform](https://www.yesodweb.com/blog/2012/03/announcing-yesod-platform)
* [Solving Cabal Hell](https://www.yesodweb.com/blog/2012/11/solving-cabal-hell)
* [Stable, Vetted Hackage: Call for Participation](https://www.yesodweb.com/blog/2012/11/stable-vetted-hackage)
* [Deprecating yesod-platform](https://www.yesodweb.com/blog/2014/08/deprecating-yesod-platform)
* [Backporting bug fixes: Towards LTS Haskell](https://www.fpcomplete.com/blog/2014/12/backporting-bug-fixes)
* [The case for curation: Stackage and the PVP](https://www.yesodweb.com/blog/2014/11/case-for-curation)
* [The brittle Haskell toolchain](https://www.yesodweb.com/blog/2015/02/brittle-haskell-toolchain)
* [Announcing stackage-update](https://www.yesodweb.com/blog/2015/04/announcing-stackage-update)
* [The true root of the PVP debate](https://www.yesodweb.com/blog/2015/09/true-root-pvp-debate)
* [The Stackage data flow](https://www.fpcomplete.com/blog/2016/04/stackage-data-flow)
* [Stackage design choices: making Haskell curated package sets](https://www.snoyman.com/blog/2017/01/stackage-design-choices)
* [Hackage Security and Stack](https://www.snoyman.com/blog/2017/02/hackage-security-stack)
* [Stackage's no-revisions (experimental) field](https://www.snoyman.com/blog/2017/04/stackages-no-revisions-field)
* [SLURP](https://www.snoyman.com/blog/2018/01/slurp)
* [Stack patching policy](https://www.snoyman.com/blog/2018/02/stack-patching-policy)
* [Haskell Ecosystem Requests](https://www.snoyman.com/blog/2018/02/haskell-ecosystem-requests)

There are also a bunch more on the FP Complete blog, but I decided
this was probably enough for the moment. Finally, I'd like to include
a write-up by Matt Parsons on [the Stack
timeline](https://gist.github.com/parsonsmatt/0d75877ea14974647621b21f8492fb43). His
write-up provides a far more concise summary than mine, with dates to
boot. What I'll describe below talks far more to the motivations for
each decision.

## Goals

I am going to state as plainly as I can the goals I have always had
for these two projects. I know many people out there have attributed
ulterior and sometimes nefarious motives to me. To those who question
my sincerity: you may as well stop reading this blog post now. For
people willing to take me at my word, here's the goal:

__Increase the adoption of Haskell.__

I work for FP Complete, and our goal is slightly different:

__Increase the commercial adoption of Haskell.__

That's really it. I haven't always made the best decisions towards
this end. In 20-20 hindsight, there are certainly other ways I would
have approached things, _especially_ on how I would have presented the
language to non-Haskellers. But that's the goal.

You may say, "Well, obviously that's the goal." But that's not
true. There are _many_ different goals that may exist. "Design the
most elegant solution to problem X," or "Generate a bunch of research
papers," or "Convince one really wealthy company to pay me a lot of
money to work on Haskell." I'm not saying those are other people's
goals, or that they are somehow other goals of mine. I'm pointing out
that the goal I had was simple to state, explains many of my
decisions, and is in fact opinionated.

One final note: I'm willing to make trade-offs to achieve that goal,
to an extent. I'm willing to giving up on some level of elegance in
the solution to solve an immediate problem. I know others disagree
with me on this. That's a fair thing to disagree on, but it also
explains why differing solutions have arisen.

## History of Stackage

The origins of this start in a simple place. I wrote Yesod. For better
or worse, I designed Yesod as a large collection of packages to allow
people to mix-and-match components (e.g., use Warp the webserver or
Shakespeare the templating engine without Yesod itself). For years, I
received many, many complaints from users about how difficult it was to
build Yesod.

I tried following the Package Versioning Policy (PVP), but at the
time, this actually made the problem worse, not better. It's really
not worth debating this point, though some will want to. This is
absolutely what I thought at the time, and why I stopped following the
PVP. I believe this was due to limitations in the dependency solver at
the time. Regardless of whether those problems have now been fixed, or
whether I just had a fundamental misunderstanding at the time, I
became convinced that a versioning policy was insufficient to escape
dependency hell.

The Haskell Platform solved some level of the dependency hell issue,
by providing a predetermined set of package versions that were known
to work well together. However, it wasn't nearly enough to solve my
problems, since it only covered a relatively small number of
packages. (This was by design, and it was the _right_ design for HP.)
To work around this, and finally stop losing prospective users, I put
together the Yesod Platform: a collection of package versions known to
work together. This helped.

When I started working for FP Complete, we ran into similar dependency
issues, as did our customers. Yesod Platform was the right idea, but
it only covered Yesod. What about people working with completely
different libraries, or using a different web framework? We cobbled
together some internal tooling for coming up with package sets.

Eventually, I realized that the entire community would be able to take
advantage of this. We created the Stackage project, started publishing
snapshots, and folded Yesod Platform into it.

Much of the community viewed Stackage as a "community wide CI," in the
sense that it gave feedback to maintainers for when their builds
failed. I thought there was much more potential to Stackage. I
believed it could dramatically increase usability of Haskell for new
users. After all, out of the gate, most Haskellers were still
regularly hitting dependency issues.

The history of Stackage continues on from here, including adding a
curator team, formalizing our processes, figuring out how to make
tricky decisions around when to boot out packages, and so on. However,
we're going to skip all of that history as it's less interesting for
now, and continue chronologically with the inception of Stack.

## History of Stack

Let me start by saying that many of the problems of cabal I'm about to
allude to have been addressed, either with improvements to the
standard workflow, the addition of sandboxes, or the `new-build`
system. However, I'm addressing the history here.

At the point our history is continuing from, `cabal-install` had
addressed some of the more serious dependency solver issues from the
past. However, dependency solving was still unreliable. You could
certainly make the claim that I was part of the problem by not
following the PVP. However, it's also irrelevant:
the design of the dependency solver till today means once you have one
piece of bad historical data, the solver cannot be guaranteed to work
correctly. I strongly believe that no matter what you do, people will
make mistakes, so trusting PVP-guided dependency solving wasn't going
to be a solution for me.

The Hackage Trustees introduced the concept of Hackage metadata
revisions at the same time. I did at the time, and still today,
strongly oppose this decision. It has made tooling much more complex,
and introduced new layers of trust into our system. Using this wasn't
an option when Stack first came onto the scene, since the system was
not fully developed (e.g., the `01-index.tar.gz` archive didn't exist
yet). There were many alternative solutions if PVP-guided dependency
solving was a goal (like ignoring previous patch releases), but we
ended up with metadata revisions instead.

Anyway, back to the history. The out-of-the-box `cabal-install`
experience involved a lot of user frustration. At FP Complete, we
ended up writing a number of scripts around `cabal-install` usage to
ensure our software could be developed and deployed reliably. We heard
the same kind of story from many other Haskell shops we spoke with. We
realized wrapping `cabal-install` was a pattern repeating across the
ecosystem.

My next goal was a simple one. Combining Stackage and cabal sandboxes,
the user experience was fairly decent, with mostly reproducible build
plans (ignoring the incoming metadata revisions). I tried to work with
the Hackage, Cabal, and Haskell Platform teams to get Stackage
included in the system. Ultimately, this resulted in a plan called GPS
Haskell, devised in person at ICFP 2014. I made some changes to
Stackage to accomodate (removing the local patching we did to work
around restrictive upper bounds). However, there was no further
response from the other teams, so I had to consider this a dead end.

At FP Complete, we were working on an FDA regulated medical device
written in Haskell. There were dozens of people working on this, many
of whom were not Haskell engineers by trade. We needed to make
guarantees to the FDA that the build plan was reproducible. For the OS
level, we used Docker. But at the Haskell layer, we needed to do
something based on Stackage. We could have done this with
`cabal-install`, but we needed something that worked reliably across
three OSes, was easy enough for non-Haskellers to use, including QA
engineers and auditors within the client company. We were looking at
writing yet another set of wrapper scripts around `cabal-install`.

This is the environment in which Stack was born. We'd already written
these scripts multiple times. We already knew how unreliable it was to
try and wrap around `cabal-install` like this. We were already
reinventing a bunch of the logic inside `cabal-install`. The presence
of the dependency solver was&mdash;for our purposes&mdash;a bug, not a
feature. We had requirements for automatic Docker support.

We discussed with our customer, and made the decision that it was time
to invest in a new tool. I promise you that this decision was not
taken lightly. For internal usage, we realized we didn't have much
choice. Releasing it as a competitor to `cabal-install` was another
matter. But it came back to the "promote (commercial) adoption of
Haskell." We had lots of anecdotal evidence to suggest that a new
build tool would help that case.

So we released Stack. It was a calculated trade-off between the
downsides of forking part of the ecosystem versus providing a better
user experience and hopefully encouraging Haskell adoption. Was it the
right decision? I still believe so. I know others do not.

I'll leave one last note in the history section, which has been a
recurring theme. No one has the ability or right to force anyone to do
anything they don't want. I could not force the `cabal-install` team
to add Stackage support. I made that request, and nothing came of
it. That's their prerogative. However, in an open source world, if you
don't listen to the requests of users, the only responses they have
are to give up on the features they want, or to fork. We elected to go
the latter route with a new tool, because the benefit of having a new
tool that did what we wanted outweighed the costs involved, at least
in our judgement.

## Philosophy

Alright, that's the history. And the goal of all of this is hopefully
clear. Over the years of building, maintaining, and modifying these
projects, the philosophy driving them has evolved and solidified. As
one person involved in these projects, let me describe the underlying
philosophy I see.

* Build plan creation should _always_ be an explicit step. There
  should never be a point where you run a command and are unexpectedly
  in build plan construction mode. One approach to ensuring this is to
  have lock files (Rust's cargo does this, among many others). Freeze
  files with cabal-install seem to go in this direction too. However,
  I'm a big fan of what we did with Stack: the available packages (aka the source map) is always
  defined in a `stack.yaml` file, and there is a separate set of
  commands (`init`, `solver`, etc) involved in modifying that
  file. Changes to your `.cabal` or `package.yaml` file have no impact
  on the set of packages available.
* Documentation guides design. There must always be a clear onboarding
  story for users, both new and those trying new things. Tools should
  be designed from that perspective. It is irrelevant to argue about
  "which tool is best for a user." The real question is: what workflow
  is best? One of the issues that has complicated discussion of Stack
  vs cabal-install is that there are multiple different cabal-install
  workflows that end up discussed concurrently. (To some extent, this
  applies to Stack as well, with scripting vs building, but I'd argue
  to a lesser extent.)
    * To get a better idea of what I mean, check out the "Next Steps"
      on the [getting started](https://haskell-lang.org/get-started)
      page. Certainly people can and do use Stack differently than
      these three links. But we've optimized for these kinds of use
      cases, and documented clearly how people can use them.
* There is an inherent, awkward piece of work in the world called
  "making the build plan." It's sometimes trivial, it's sometimes
  difficult. The work is inherent, though different approaches can to
  some extent make the work larger or smaller.  The Stackage approach
  is based on curation. It's a tried-and-tested approach from the
  Linux distribution world, and it centralizes a lot of the pain and
  work. Dependency solving is a more flexible and distributed
  approach. However, it depends on the cooperation, compliance, and
  correctness of a large group of people, and/or a large amount of
  work by a centralized group to edit the metadata. As a result, my
  personal view is:
    * We should focus on curation for the most common use case
    * For the cases where, for whatever reason, curation ends up not
      being sufficient, falling back to manual build plan construction
      is painful, but limited in scope. I [gave an
      example](https://www.reddit.com/r/haskell/comments/9bkd6m/errors_when_installing_grapefruit_with_stack/e5579f9/?context=3)
      of doing this on Reddit.
* Reproducibility is a great goal, but there's _always_ a limit to how
  reproducible you can be. Stack aims for reproducible build plans,
  but nothing at the OS level. Nix goes a step farther, but doesn't
  for example ensure identical hardware. My point is: define a level
  of reproducibility you want from the tool at hand and strive to
  ensure it. I think Stack has the right idea for a build tool, and
  OS-level reproducibility should be handled externally, such as via
  Docker, Nix, or VMs.
* It is vital to observe the new user experience and respond to
  it. I'm still upset (with myself) that there were so many potential
  Haskell and Yesod users who almost immediately dropped the language
  because of the bad onboarding experience. If I have one biggest
  regret in all of this, it's that I wasn't more proactive in looking
  for a solution to these problems far earlier. In other words, I wish
  I'd thought of Stackage and Stack about 5 years before I did!
* There will be use cases we don't address, and that's fine. Stack's
  goal is not, has never been, and (if I have my way) never will be,
  to defeat all other build tools. Stack has a defined set of use
  cases it works well for. If it doesn't distract and people want it,
  we can add additional such use cases. But the existance and
  improvement of other tools is a Good Thing.
* We've had a running philosophy in Stack of including extra
  functionality, such as built in Docker and Nix integration. But see
  the future section for some evolved thoughts on this.
* Decouple and avoid needless forking. I obviously made some decisions
  that include forking. I've also been very careful to make those fork
  points as minimal as possible. Stackage and Stack both use Hackage
  as a package repository, instead of having their own. Both projects
  continue to use Cabal (the file format, build system, and library)
  instead of creating a completely separate build system. I can assure
  you, there were strong technical arguments to do things differently,
  and there have been large, ongoing costs (to me personally in many
  cases) for having _not_ forked. But overall, I believe in the goal
  of decoupling the selection of tooling, and making the build tool
  selection orthogonal to the package store is a Good Thing.

## Future

One thing we've already been doing in the Stack code base is
decoupling extra functionality. We've split off a few libraries
already. I've spent some time working on Pantry as a means to separate
out the package source logic from Stack. I'd like to see that trend
continue. Another idea is [separating out the GHC installation
logic](https://github.com/commercialhaskell/stack/issues/4261) so it
can be used separately. In my ideal world, "how you install the tools"
would be orthogonal to "which tools you use." It's part of why I
ultimately opposed to the original Haskell Platform (now HP Full): it
bundled opinionated concepts about which libraries you should use with
the less opinionated installer for GHC.

There's [open
work](https://github.com/commercialhaskell/stack/issues/4217) on
improving the `stackage-curator` tool. This follows directly from the
Pantry work, and will hopefully allow a few improvements:

1. Make it easier for people to create their own snapshots. I strongly
   believe in decoupling, and allowing others to create their own,
   opinionated snapshots separate from Stackage Nightly and LTS
   Haskell would be great.
2. Make it easier to integrate with Nix. I know they have some scripts
   in place already to use Stackage snapshots, and I'm aware of at
   least one shortcoming (not respecting the Hackage revision pinned
   by Stackage).
3. Make the process of choosing which packages to hold back in LTS
   minor bumps more transparent. The current tooling makes it hard to
   be clear about what decisions were made.

For security sensitive use cases (like the FDA regulated medical
device I mentioned above), including more secure source information is
vital. Currently, Stack relies upon the package tarball hashes from
Hackage Security. This introduces a layer of trust that those hashes
are not modified or corrupted over time, which is hard to justify in
highly-security-sensitive setups. The upcoming Pantry-based Stack will
include far more hashing information so that you can demonstrate
provenance of the source files exclusively from what's in your source
tree. I want to see further improvements to the security story like
this.

As you may have noticed, I've talked a lot about community processes
over the past year or so. I find the area fascinating and
underexplored. I'm already trying to take a back seat in the code
writing aspect of these projects, and involve myself more in
organization and support. The Stackage Curator team has worked out
really well, and the much newer Stack Issue Triagers initiative seems
to be working out nicely as well.

### Collaboration with cabal?

As a final point, let me address this question, which comes up a
lot. Despite what many people claim, neither FP Complete nor I have
any vested interest&mdash;or even desire&mdash;to maintain a separate
build tool (Stack). Stackage is a project I started because I saw a
gap in the ecosystem. But my preference had always been to have
Stackage become part of the rest of the tooling, not have a separate
set of tooling.

At this point, many people and companies rely on the Stack tooling and
workflow. `cabal-install` has certainly made improvements in the past
5 years, but it is still different. It has different goals, a
different UX, and supports different features. There is not a 1-to-1
mapping between the functionality.

I'd love to see the gap close. I'd still love to see support for
Stackage snapshots added to `cabal-install`. I think Stack has done some
great work with things like the script interpreter, the approach we
have to the implicit global project, the reproducible-by-default build
plans, and the security we're introducing with Pantry.

In my ideal world, and I mean this with complete sincerity, enough of
these features would move into `cabal-install` that we could declare
Stack no longer needed. While forking isn't inherently evil, if it can
be avoided it would be a good thing. I don't think the current plans
for `cabal-install` take it in the direction I'd like to see, so this
is unlikely to happen. But I say in all good faith: if enough of the
Stack community's needs and desires could be met with `cabal-install`,
I'd advocate for a some kind of deprecation cycle for Stack.
