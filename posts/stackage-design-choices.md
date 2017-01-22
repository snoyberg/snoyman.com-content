This post is going to talk about some of the design choices made over
the years around the
[Stackage project](https://github.com/fpco/stackage#readme), a curated
package set for Haskell. While many of these points will be Haskell-
and Stackage-specific, I think the ideas would translate well to other
languages interested in created curated package sets. This blog post
was inspired by a short discussion on Twitter, which made it clear
that I'd never really shared design thoughts on the Stackage project:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr"><a href="https://twitter.com/snoyberg">@snoyberg</a> <a href="https://twitter.com/iElectric">@iElectric</a> sounds like you&#39;ve put already a lot of thought into that. would luv to learn more about!</p>&mdash; Haskell Dev (@haskdev) <a href="https://twitter.com/haskdev/status/821085009763246081">January 16, 2017</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

In understanding why Stackage is the way it is today, it will be
important to take into account:

* The goals of the project
* The historical circumstances when decisions were made
* Social pressures in the community agitating for specific decisions
* Inertia in the project making significant changes difficult

Apologies in advance, this turned out longer than I'd intended.

## Goals

Before Stackage, the most common way to find a set of libraries to use
in a Haskell project was using cabal-install's dependency solver,
based on bounds information specified by authors. There were certainly
some efforts at creating curated package sets previously (Haskell
Platform provided a limited set; the Yesod Platform provided a full
set of packages for the Yesod Web Framework; various Linux distros had
binary packages). But I think it's fair to say that the vast majority
of people writing Haskell code were using dependency solving.

I'm not going to get into the argument of dependency solving vs
curation here. I will simply say that for many people - myself
included - having official combinations of packages which are known to
compile together, and which can be given to end users and teammates on
a project, was very appealing. This was the motivation for my
[initial Stackage call for participation](http://www.yesodweb.com/blog/2012/11/stable-vetted-hackage).

While the primary goal - create curated package sets - is obvious, the
secondary goals are not. In fact, many of them only really became
clear to me in 20-20 hindsight:

* Require as little maintenance as possible. Stackage should be as
  much an automated process as can be created, since human time is a
  valuable, scarce resource. In other words: I'm lazy :).

* Require as little change in behavior from package authors as
  possible. In my opinion, the only reasonable way to bootstrap a
  project is to make it trivial for people to participate. The barrier
  to entry for Stackage had to be minimal.

    * Even past the "bootstrapping" phase, a nice quality of _any_
      system is requiring little effort on the part of
      users. Therefore, even today, where Stackage is arguably
      successful and well-established, this goal still applies.

* It needed to work well with existing tooling. In 2012, the Stack
  project hadn't even been dreamt up yet, so figuring out a way to
  work with cabal-install (via the cabal.config file) was
  vital. Compatibility with cabal-install is still a nice thing today,
  but not nearly as vital as it was then.

*   We need to maximize the number of packages that can be included in
    a single snapshot. The two ways in which two packages can be
    incompatible are:

    * There is an actual incompatibility in the API, such as a
      function being removed or its type signature changed in a new
      release of a dependency.

    * There is a stated upper or lower bound in a package which
      precludes a build plan, but the code itself would actually
      compile. (This is the case `--allow-newer` is designed for.)

### Initial choices

Based on these goals, I created the initial version of Stackage. While
many decisions came into play (e.g., what file format should we use to
let package authors submit packages?), I'm going to focus on the
interesting choices that fell out of the goals above, and which today
may be noteworthy.

* As I'd learnt from maintaining Yesod, many Windows users in
  particular were using the Haskell Platform (HP), and trying to
  specify different versions of packages from what HP provided could
  cause problems. Therefore, it was important to keep compatibility
  with the Haskell Platform set of packages. This resulted in multiple
  builds of Stackage: a "current GHC", "previous GHC", and "Haskell
  Platform superset."

* We should always try to take the latest available version of a
  package, as it may include bug fixes, feature enhancements, and
  generally because the Haskell community loves the bleeding edge
  :). However, there would be cases where a new version of a package
  caused enough breakage to warrant holding it back, so some concept
  of enforced upper bounds was necessary too.

*   It was theoretically possible to ignore version bound information
    in cabal files, and instead ensure compatibility based on
    compiling and running test suites. However, this would have some
    serious downsides:

    * Users would have regularly needed to run builds with
      `--allow-newer`
    * If there were non-API-breaking semantic changes in a package, a
      version bound was present to avoid those changes, and there was
      no test suite to cover that behavior, ignoring bounds would
      cause those semantic changes to slip in (in my experience, this
      is an exceedingly rare case, but it can happen)
    * It's arguably very confusing behavior that a package set
      specifies versions of packages which claim to be incompatible
      with each other

    Therefore, version bounds needed to be respected. However...

* Due to the frequency of overly restrictive version bounds and
  trivial compatibility patches which were slow to make it upstream,
  Stackage allowed for locally modified packages. That means that, for
  example, Stackage snapshot `foo` could have a different set of code
  associated with `mtl-2.2.1` than what Hackage reports. Note that
  this feature was more aggressive than Hackage cabal file revisions,
  in that it allowed the code itself to change, not just the cabal
  file.

These decisions lasted for (IIRC) about a year, and were overall
successful at letting Stackage become a thriving project. I was soon
able to [shut down the Yesod Platform initiative](http://www.yesodweb.com/blog/2014/08/deprecating-yesod-platform)
 in favor of Stackage,
which was a huge relief for me. At this point, outside of the Yesod
community, I think Stackage was viewed mostly as a "ecosystem-wide CI
system" than something for end users. It wasn't until Stack defaulted
to Stackage snapshots that end users en masse started using Stackage.

## Changes over time

Stackage today is quite a bit different from the above decisions:

* I eventually dropped the Haskell Platform superset. There was a
  time when that package set wasn't updated, and the complication of
  trying to find a compatible set of packages on top of it was simply
  too high. In addition, HP included a version of aeson with a
  significant security hole (DoS attack with small inputs), and
  continuing to supply such a package set was not something I felt
  comfortable doing.

* Due to the burden of maintaining bleeding-edge Stackages for
  multiple GHC versions - both on myself as the curator and on package
  authors - I also dropped support for older GHC releases. Instead, I
  introduced LTS Haskell, which keeps compatibility with older GHCs
  without adding (significant) package author burden.

* When working on the GPS Haskell collaboration, I removed support for
  locally modified packages. This was done due to requests from the
  Hackage and Haskell Platform maintainers, who wanted a single
  definition of a package. With this change, unresponsive package
  maintainers can really hold things up in Stackage. However, this
  overall led to a number of simplifications in code, and ultimately
  allowed for better binary cache support in Stack. So despite the
  initial pain, I think this was a good change.

* Hackage revisions make it possible for a package set to contain
  packages which are no longer compatible by their latest cabal
  files. Therefore, we needed to add support to Stackage to track
  which version of a cabal file was included in a snapshot, not just
  the version of the package itself. I only mention this here because
  it weakens our previous decision to respect cabal file constraints
  due to avoiding user confusion.

* We have an expanded team! I'm happy to say that I am now one of five
  Stackage curators, and no longer have to either handle all the work
  myself, or make unilateral decisions. In other words, I get to share
  the blame with others :). Many thanks to Adam Bergmark, Dan Burton,
  Jens Petersen, and our newest member, Luke Murphy.

## Changes to consider today

Alright, this post has turned out way longer than I'd expected,
apologies for this. I guess there was more decision making that
occurred than I'd realized. Anyway, I hope that gives some context for
where things are at today. Which brings us to the original discussion
that brought this whole blog post into existence: should we be
changing anything about Stackage? Here are some changes either
proposed by others or that I've thought of, and some remarks.

* The curator team overall has been pretty lax about booting packages
  that block newer versions of dependencies. There have definitely
  been calls for us to be more proactive about that, and aggressively
  kick out packages that are holding back dependencies.

    * Pros: Stackage Nightly will live up to its bleeding edge mission
      statement more effectively, we'll overall have less incidental
      pain on package authors who are staying up to date with their
      dependencies.

    * Cons: it will decrease the number of packages in Stackage
      Nightly for end users, and adds extra burden on package authors
      to be more quick to respond to requests.

* As a relaxed version of the above: be stricter with package authors,
  but only in the case of cabal file upper bounds. The argument here
  is stronger, since the work required is fairly minimal, and - at
  least in my experience - waiting for relaxed upper bounds is what
  takes up a lot of the time when curating. An extreme version of this
  is demanding that
  [upper bounds just be removed](https://twitter.com/haskdev/status/820932892385808384).

* Or an interesting alternative to that: should Stackage simply ignore
  constraints in cabal files entirely? It would be fairly easy to
  extend Stack to recognize a flag in snapshots to say "ignore the
  constraints when building," or even make that the default behavior.

    * Pros: less time spent on bounds issues, Stackage doesn't get
      held back by trivial version bounds issues, for PVP bounds
      enthusiasts could encourage people to add bounds during upload
      more often (not sure of that).

    * Cons: cabal users with Stackage snapshots wouldn't have as nice
      a time, it could be confusing for users, and if the upper bounds
      are in place due to semantic changes we won't catch it.

* Since GPS Haskell isn't happening, we could add back the ability for
  the Stackage curator team to modify packages (both cabal files and
  source files). I think the pros and cons of this were pretty well
  established above, I'm not going to repeat it here.

* People have asked for running multiple nightly lines with different
  GHC versions.

    * Pros: instead of haven't slightly outdated LTS versions for
      older GHCs, we'd have bleeding edge all over again.

    * Cons: we'd need new naming schemes for snapshots, a lot more
      work for the curator team, and potentially a lot more work for
      package authors who would need to maintain further GHC
      compatibility with their most recent releases.

* I've had some private discussions around this, and thought I should share the
  idea here. Right now, Stackage requires that any package added must be
  available on Hackage. A number of newer build systems have been going the route
  of allowing packages to be present only in a Git repository. Stack has built-in
  support for specifying such locations, but snapshots do not support it. Should
  we add support to Stackage to allow packages to be pulled from places besides
  Hackage?

    * Pros: knocks down another barrier to entry for publishing packages.

    * Cons: Stackage snapshots will not automatically work with cabal-install
      anymore, extra work to be done to make this functional, and some issues
      around determining who owns a package name need to be worked out.

There are likely other changes that I haven't mentioned, feel free to
raise them in the comments below. Also, if anyone really wants to
follow up on these topics, the best place to do that is
[the Stackage mailing list](https://groups.google.com/d/forum/stackage).
