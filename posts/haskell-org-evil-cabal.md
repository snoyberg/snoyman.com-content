There's no point being coy or saying anything but what I actually
believe, and saying it bluntly. So here it is:

> The haskell.org committee has consistently engaged in tactics which
> silence the voices of all non-members, and stacks the committee to
> prevent dissenting opinions from joining.

I've said various parts of this previously. You may have heard me say
things like the haskell.org oligarchy, refer to the "evil cabal of
Haskell" (referring to the nepotism which exists amongst Hackage,
cabal-install, haskell.org, and the Haskell Platform), or engage in
lengthy debates with committee members about their actions.

_This is a pretty long post, if you want to see my request, please
[jump to the end](#my-request)._

## The backstory

To summarize a quick backstory: many of us in the community have been
dissatisfied with the four members of the "evil cabal" for years, and
have made efforts to improve them, only to be met with opposition. One
by one, some of us have been replacing these components with
alternatives. Hackage's downtime led to an
[FP Complete mirror](https://www.fpcomplete.com/blog/2015/03/hackage-mirror)
and more reliable doc hosting on
[stackage.org](https://www.stackage.org). cabal-install's weaknesses
led to the creation of
[the Stack build tool](https://haskellstack.org). Haskell Platform's
poor curation process and broken installer led to Stackage Nightly and
LTS Haskell, as well some of the Stack featureset. And most recently,
the haskell.org committee's poor decisions (as I'll demonstrate
shortly) for website content led to resurrecting
[haskell-lang.org](https://haskell-lang.org/), a website devoted to
actually making Haskell a more approachable language.

As you can see, at this point all four members of the evil cabal have
been replaced with better options, and community discussions and user
statistics indicate that most users are switching over. (For an
example of statistics, have a look at the
[package download count on Hackage](http://hackage.haskell.org/packages/top),
indicating that the vast majority of users are no longer downloading
packages via cabal-install+Hackage.) I frankly have no problem at all
with the continued existence and usage of these four projects; if
people want to spend their time on them and use what I consider to be
inferior tools, let them. The only remaining pain point is that new,
unsuspecting users will arrive at
[haskell.org download page](https://www.haskell.org/downloads) instead
of the much more intuitive
[haskell-lang.org get started page](https://haskell-lang.org/get-started).

## The newest attempt

Alright, with that out of the way, why am I writing this blog post
now? It's due to
[this post on the Haskell-community mailing list](https://mail.haskell.org/pipermail/haskell-community/2016-August/000118.html),
proposing promoting the Haskell Platform above all other options (yet
again). Never heard of that mailing list? That's not particularly
surprising. That mailing list was created in response to a series of
complaints by me, claiming that the haskell.org committee acted in a
secretive way and ignored all community input. The response to this
was, instead of listening to the many community discussions already
occuring on Twitter and Reddit, to create a brand new mailing list,
have an echo chamber of people sympathetic to Evil Cabal thought, and
insist that "real" community discussions go on there.

We're seeing this process work exactly as the committee wants. Let me
demonstrate clearly how. At the time of writing this blog post, three
people have voted in favor of promoting the HP on haskell-community,
including two haskell.org committee members (Adam Foltzer and John
Wiegley) and the person who originally proposed it, Jason Dagit. There
were two objections: Chris Allen and myself. So with a sample size of
5, we see that 60% of the community wants the HP.

## The lie

A few hours after this mailing list post, I put out a
[poll on Twitter](https://twitter.com/snoyberg/status/769732460879962112). At
time of writing (4 hours or so into the poll), we have 122 votes, with
85% in favor of Stack, and 15% in favor of some flavor of the Haskell
Platform (or, as we'll now be calling, the
[Perfect Haskell Platform](https://twitter.com/snoyberg/status/769752895482896384)). Before
anyone gets too excited: yes, a poll of my Twitter followers is
obviously a biased sample, but no more biased than the
haskell-community list. My real point is this:

__The haskell.org committee is posing questions of significant
importance in echo chambers where they'll get the response they want
from a small group of people, instead of engaging the community
correctly on platforms that make participation easy.__

This isn't the first time this has happened. When we last discussed
haskell.org download page content, a similar phenonmonon
occurred. Magically, the haskell-community discussion had a bias in
favor of the Haskell Platform. In response, I created a Google Form,
and Stack was the clear victor:

<a href="https://docs.google.com/forms/d/1w2wKSxn5YN4LtSXYHvFT2IFw_BDaT_2cjUkP9pDeqLQ/viewanalytics"><img src="https://i.sli.mg/cGLAWL.png" width="100%"></a>

Yet despite this clear feedback, the committee went ahead with putting
minimal installers at the top, not Stack (they weren't quite brazen
enough to put the Perfect Haskell Platform at the top or even above
Stack, for which I _am_ grateful).

## Proper behavior

As I see it, the haskell.org committee has two correct options to
move forward with making the download page decision:

* Accept the votes from my Twitter poll in addition to the haskell-community votes
* Decide that my poll is invalid for some reason, and do a proper poll
  of the community, with proper advertisement on Reddit, Twitter, the
  more popular mailing lists, etc

If past behavior is any indication though, I predict a third outcome:
stating that the only valid form of feedback is on the
haskell-community mailing list, ignore the clear community groundswell
_against_ their decisions, and continue to make unilateral, oligarchic
decisions. Namely: promote the Haskell Platform, thereby misleading
all unfortunate new Haskellers who end up at haskell.org instead the
much better haskell-lang.org.

## Further evidence

Everyone's always asking me for more of the details on what's gone on
here, especially given how some people vilify my actions. I've never
felt comfortable putting that kind of content on blogs shared with
other authors when some of those others don't want me to call out the
negative actions. However, thankfully I now have my own blog to state
this from. This won't include every punch thrown in this long and
sordid saga, but hopefully will give a much better idea of what's
going on here.

*   Not only are conversations held in private by the committee, but:

    * Their private nature is used to shut down commentary on committee actions
    * There is open deception about what was actually discussed in private

    Evidence: see
    [this troubling Reddit thread](https://www.reddit.com/r/haskell/comments/4ruqbl/new_haskell_community_nexus_site_launched/d57xtbu). I
    made the (very true) claim that Gershom made a unilateral decision
    about the downloads page. You can see the evidence of this
    [where he made that decision](https://github.com/haskell-infra/hl/pull/122). Adam
    Foltzer tried to call my claim false, and ultimately
    [Gershom himself confirmed I was correct](https://www.reddit.com/r/haskell/comments/4ruqbl/new_haskell_community_nexus_site_launched/d582jzk). Adam
    then claimed offense at this whole discussion and backed out.

*   When I
    [proposed making Stack the preferred download option](https://github.com/haskell-infra/hl/pull/130)
    (at a time when Stack did not appear _at all_ on haskell.org),
    Gershom summarilly closed the pull request. I have referenced this
    pull request many times. I don't believe any well intentioned
    person can read that long discussion and believe that the
    haskell.org committee has a healthy process for maintaining a
    community website.

*   At no point in any of these discussions has the committee opened
    up discussion to either the technical advantages of the HP vs
    Stack, or the relative popularity. Instead, we get discussions of
    committee process, internal votes, an inability to make changes at
    certain periods of time based on previously made and undocumented
    decisions.

*   We often hear statements from committee members about the strong
    support for their actions, or lack of controversy on an
    issue. These claims are many times patently false to any objective
    third party. For example,
    [Gershom claimed](https://www.reddit.com/r/haskell/comments/4ruqbl/new_haskell_community_nexus_site_launched/d582jzk)
    that the pull request #122 that he unilaterally decided to merge
    was "thought to be entirely mundane and uncontroversial." Everyone
    is welcome to
    [read the Reddit discussion](https://www.reddit.com/r/haskell/comments/3b1yuk/haskellinfrastructure_fwd_new_haskell_platform/)
    and decide if Gershom is giving a fair summary or not.

*   Chris Done - a coworker of mine - spent his own time on creating
    the first haskell-lang.org, due to his unhappiness with
    [the homepage at that time](https://wiki.haskell.org/Haskell). His
    new site was met with much enthusiasm, and he was pressured by
    many to get it onto haskell.org itself. What ensued was almost a
    year of pain working out the details, having content changed to
    match the evil cabal narrative, and eventually a rollout. At the
    end of this, Chris was - without any given reason - not admitted
    to the haskell.org committee, denying him access to share an
    opinion on what should be on the site he designed and created.

## My request

Thank you for either getting through all of that, or skipping to this
final section. Here's my request: so many people have told me that
they feel disenfranchised by these false-flag "community" processes,
and just give up on speaking up. This allows the negative behavior
we've seen dominate the evil cabal in Haskell for so long. If you've
already moved on to Stack and Stackage yourself, you're mostly free of
this cabal. I'm asking you to think of the next generation of Haskell
users, and speak up.

Most powerful course of action: subscribe to the
[haskell-community mailing list](https://mail.haskell.org/cgi-bin/mailman/listinfo/haskell-community)
and speak out about how the committee has handled the downloads
page. Don't just echo my message here: say what you believe. If you
think they've done a good job, then say so. If you think (like I do)
that they've done a bad job, and are misleading users with their
decisions, say that.

Next best: comment about this on Reddit or Twitter. Get your voice out
there and be heard, even if it isn't in the haskell.org committee echo
chamber.

In addition to that: expect me to put out more polls on Twitter and
possibly elsewhere. Please vote! We've let a select few make damaging
decisions for too long, make your voice heard. I'm confident that we
will have a more user-friendly Haskell experience if we actually start
listening to users.

And finally: as long as it is being mismanaged, __steer people away
from haskell.org__. This is why we created haskell-lang.org. Link to
it, tell your friends about it, warn people away from haskell.org, and
maybe even
[help improve its content](http://github.com/haskell-lang/haskell-lang#contributing-content).
