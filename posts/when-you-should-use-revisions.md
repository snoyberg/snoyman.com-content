It's pretty well known that Hackage Revisions&mdash;which allow
revising your package's cabal file on Hackage&mdash;are an often
debated feature, and one that I've unabashedly come out against. In a
recent discussion with a package maintainer, I realized that I needed
to clarify my position on revisions. While I think the ecosystem would
be better off without them, given that they exist and are relied upon
for some functionality, there are absolutely times you _should_ use
them. This blog post is some examples of those.

**NOTE** I'm intentionally not rehashing the reasons for and against
revisions, as it's irrelevant to this post. I'm simply pointing out
that for those people out there who do not like the revision system,
you may still want to use it, and you shouldn't feel guilty doing
so. I'm asking everyone _not_ to turn this into an excuse for a
flamewar.

## Bad package upload

Let's say you upload a version of a package with a massive security
hole, or with some debug code in it by mistake, or one of another
dozen accidents. You need to mark this release as something that
should not be used. You may be tempted to use the preferred versions
feature of Hackage, but this won't work out as you'd hope. The cabal
dependency solver (to the best of my memory) treats preferred versions
as soft constraints, and therefore may still choose the offending
package version.

Instead, you can work around this by using a revision to put an
impossible version bound on your package. If I remember correctly,
`base < 0` is the commonly used one.

__Downsides__

* Error messages can be confusing
* This will break any existing build plan using this package version;
  whether that's a good thing or bad thing is a matter of opinion. For
  what it's worth, if you use Stack's new revision pinning, your old
  build plan will continue to work

For a planned alternative solution in the future, see the [security hole and
major bug tracking
proposal](https://www.reddit.com/r/haskell/comments/4uta1e/proposal_tracking_security_holes_and_major_bugs/)
which in my opinion would address this case better. Of course, this isn't
available yet, and so shouldn't affect your decisions today.

## Incorrect version bounds

There are a number of different kinds of incorrect version bounds you
could have. One is overly strict upper or lower bounds, e.g., you work
with `foo-1.2.3`, but your dependencies say `foo < 1.2`. In this case,
there's no need to use revisions, you can simply upload a new version
of your package to Hackage. However, given that revisions are a
reality, I don't really see the harm in adding yet another revision.

The other incorrect version bounds are overly lax bounds. This can
happen for at least three reasons:

* You're not following PVP bounds policies, and are ommitting either
  upper bounds, lower bounds, or both
* You made a mistake in the specification of your bounds, e.g. you
  thought you supported `bytestring >= 0.9`, but it turns out you're
  using a feature in `bytestring-0.10`.
    * Side note: my guess is that these kinds of accidentally lax
      lower bounds happen a _lot_
* An upstream dependency adds breaking changes in a minor version bump
  (accidentally or intentionally, doesn't matter)

You've got three (non-mutually-exclusive) options of what to do next.

* Release a new minor version/patch version of your package that fixes
  compatibility. In other words: leave the bounds in the cabal file
  exactly the same, but ensure that the build failure does not occur
  by modifying the code. I _strongly_ recommend doing this whenever
  you can. Keeping version ranges as wide as possible is a great help
  in avoiding dependency hell problems.
* Release a revision of the existing cabal files to tell tools not to
  build the offending combination. You may need to revise many
  different versions of the cabal file. This will keep the dependency
  solver happy.
* Don't worry about the dependency solver case, and accept that
  someone trying to combine older versions of packages together will
  get some breakage. If you follow this route, I again strongly
  recommend ensuring that at the very least the latest version of your
  package builds with all of the latest versions of its
  dependencies. ([Keeping your packages in Stackage](https://github.com/fpco/stackage/#add-your-package)
  is a good way to ensure this.)

## Having fun

This one was too much fun not to share publicly:
<https://github.com/snoyberg/acme-year-revisions>. You can of course
use custom `Setup.hs` files to make your build results dependent on
cabal file revisions, so go ahead and use revisions to fix up your
code itself.

NOTE: Don't do this.

## Summary

Again, the point here isn't to relitigate revisions. The point is to
accept that they are currently the only option available for some
cases, and in others they happen to be convenient even if
unnecessary. Even if you're not a fan of revisions (like me), you're
not betraying your core beliefs by using revisions in these cases.

## Other cases?

I'm going to admit that I wrote this blog post in an airport running
on very little sleep. I could have sworn I had other cases in mind. If
you think of examples where revisions should definitely be used, let
me know, and I'll try to update the post accordingly.
