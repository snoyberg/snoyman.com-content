Many people in the community have seen the SLURP proposal. Some people
have asked my opinion. Some others have made some... let's say
_colorful_ statements about my noninvolvement in the discussion. Let
me set the record straight right now on why I've avoided the
topic. The authors showed me the proposal before it was published, and
I told them at that time I would not support it. I've also told them
that, out of respect to them, I would hold back on commenting on
SLURP. Unfortunately, that's now led to two things:

* Some people making some very pointed implications
* Misunderstanding about the usage of the term "fork" in the proposal,
  which unfortunately the authors have not rectified

To be clear: the proposal is _not_ mine, I did _not_ ask for this
change, and I'm not "holding a gun" to anyone's head. Those
descriptions aren't true.  There are plenty of other statements I
could comment on as well, but it's honestly not worth it.

Here's what isn't false: I regularly am in communication with many
people across the Haskell community and ecosystem management teams
about problems being faced. I interact with a broad group of users in
my work, hear complaints, and relay them. I have my own complaints,
and relay those as well. Some of these complaints have all pointed in
similar directions.

My hands are tied on what I can say publicly, since so many comments
are made in private emails that people object to being made
public. And I know (from experience) that there are detractors out
there who will straight out accuse me of lying. I've been avoiding
saying anything because of this constant accusation, but I've decided
to just put the info out there. I figure:

* People who want to believe everything I do is malicious won't care
  if I have evidence anyway
* People who are open to the possibility that I'm not evil will
  hopefully take my statements at face value

One last foreword: I used to very openly discuss my thoughts on
architecture and ecosystem development. I believe it's the only real
way to build an open source community. When tensions got their highest
in the Stack-vs-cabal days, many people rebelled against this public
broadcast methodology, and I've switched to quieter communication
channels. I think this is unfortunate, and I'd much rather talk openly
and loudly about ecosystem plans and let people have easy ways of
input. I object strongly to the mentality of discussing everything
behind closed doors. We'll see if open discussions can resume at some
point.

## What's the fork?

It seems clear to me now that the vast majority of discussion on SLURP
has nothing to do with SLURP itself, but with its comments about
forking. I really do wish that the authors had been willing to speak
to that publicly if they were going to use the term fork in the
document. I will speak to what I know about forking in the Stackage
and Stack worlds. We'll have to leave it to the authors to speak for
themselves as to whether my words here reflect what they'd intended.

The term "fork" here is definitely not being used in its most literal
sense of "taking a software project, hosting the source code
elsewhere, then continuing development under a different name" (my
made up definition). It's referring to a more general split. Stack is
called by many a fork of cabal-install, for example, even though they
share no code (they share underlying libraries, like Cabal, of
course).

Since everyone is most fixated on this point, let me state it clearly:
I have been involved in absolutely 0 conversations where anyone wanted
to host a direct competitor to Hackage. At all. No one I know wants to
do this. I don't want to do this. Stackage and Stack today feed from
Hackage, and no one I know wants to change that. No one I know wants
to try to take over control of Hackage, for that matter.

When "fork" of Hackage is mentioned, that seems like the most logical
conclusion to draw. I can guarantee that it's not the case.

Now let me address some concrete pain points that may lead to some
kind of "fork."

## Hackage Revisions

Many people are very outspoken about their dislike for Hackage
Revisions. I dislike Hackage Revisions. I have more reason than most
to dislike them: I've invested weeks to months of my life making
changes to multiple tools to support revisions. I could go through the
gory history of this, but it's not worth it: it would just be a
programmer's war stories session. So let's turn to today.

With Stack 1.6, I finally got all of the pieces in place to fully
support revision pinnings. Stackage has already had revision pinning
for a long time. Stackage has the ability to list some packages as
ignoring revisions.

If you ask me today, I will still say revisions are a bad idea, they
should be disabled, and better solutions to the dependency resolution
problem implemented (I've discussed those at length in the past). At
the same time: the cost is now sunk. I still worry about the fact that
users do not, in fact, pin their extra-deps to specific revisions, and
that the rules for revisions on Hackage are far too lax. These a real
concerns that I care about, but also not the top of my personal
priority list.

Others, by the way, feel differently. I know many individuals who are
offended at the thought of a Hackage Trustee forcibly editing their
cabal files. I don't disagree with them per se, but I'm also not as
passionate about this topic. In conversations with community leaders,
I've made this distinction very clear (at least, I've _tried_ to make
it clear).

My biggest remaining concern about revisions is the social implication
they carry. Namely: the idea that someone else is responsible for the
stability of your build. I've mentioned many times that I believe a
huge source of our social tension is a world where you can complain to
an upstream developer because your build suddenly stopped
working. That's a recipe for disaster, and is a fundamental flaw in
the PVP+dependency solving world. We need tooling that focuses instead
on fixed build plans. I've advocated for this for years, and
ultimately created Stack largely due to inability to get traction
upstream.

In sum: will revisions lead to anything of a fork? No.

## Curation

A few weeks ago I tweeted:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">I did that initially. When collaborating on GPS Haskell, I removed that functionality as a requirement of the Hackage, Cabal, and Haskell Platform teams. Then GPS died and we&#39;re stuck unable to work around upstream breakage like this.</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/949385673982730240?ref_src=twsrc%5Etfw">January 5, 2018</a></blockquote>

The original design of Stackage followed a standard Linux distribution
model directly. Hackage was our upstream, we maintained a set of
patches to avoid massive version bound disruption, and _very_
occasionally (if at all, I honestly don't remember) edited source
files to fix bugs.

In 2014, when I discussed the plans for incorporating Stackage into
cabal and the Haskell Platform (code named GPS Haskell, and which
never got off the ground), the cabal, Hackage, and HP maintainers
required that Stackage not maintain any local modifications. I removed
that functionality, and that's the world we've been in since.

Adding that back _is_ on the table. I'll explain why in a second. This
could be considered a fork, and some may call it a soft fork. It's
honestly not a feature I want to add back to Stackage, since
maintaining patch sets is a lot of work. But many communities need to
do it. As I understand it, Nix does it as well. So if it's a fork,
it's a fork we already have widely in our ecosystem.

One "nice to have" reason for adding in this curation is to work
around packages which are slow to upgrade to newer dependency
versions. It can be very frustrating for Stackage package maintainers
to have their packages held back because _someone else_ won't relax an
upper bound. Curation would let us work around that. I consider this a
perk, but not a necessity.

But the more important reason for this is to deal with packages which
are causing trouble in the Stackage or Stack world, but are not
causing trouble in the cabal-install world. I didn't consider this a
real concern until it happened multiple times in the past few
months. You can
[see an example here](https://github.com/haskell-hvr/cassava/pull/155).

I'm not demanding anything of any authors by making this
statement. But here's the reality: I personally end up spending a lot
of my own time dealing with these kinds of breakages. My friends and
colleagues get sucked into various carry-on tasks, like cutting new
emergency point releases. I do not want my life to be spent in a
situation where, at a moment's notice, I'll need to dedicate large
amounts of time to changing something in Stack to be compliant with
something in the Cabal library which _should_ be in a spec, but is
instead undocumented.

Hackage already takes great pains to ensure it does not break
cabal-install. Many people have probably heard about how the `^>=`
operator's introduction broke Stack 1.5. What many people _didn't_
hear about is that it also broke cabal-install 1.24. You didn't hear
about it, because
[Hackage implemented a workaround to hide those files from older cabal-install versions](https://github.com/haskell/cabal/issues/4624). This
curation idea is to provide a way for Stackage to work around breakage
for Stack, the same way Hackage will work around damage for
cabal-install.

And yes: I requested that the same kind of treatment be given to Stack
from Hackage. That was met with calls of asking for preferential
treatment. Readers can determine what they feel.

In sum: I'm working towards allowing Stackage to apply patches to
upstream packages. I don't consider this a fork, but rather
curation. Others may choose to label it a fork.

## Avoid uploading to Hackage

I'll start with this: my personal preference is to continue uploading
all of my packages to Hackage. I have no intention nor desire to stop
uploading conduit, yesod, or any of the other 80+ packages I actively
maintain to Hackage. That said, not everyone feels the same way.

Today, Stackage is strictly downstream of Hackage. You cannot get a
package into Stackage unless it is first uploaded to Hackage. Period,
end of story. There seem to be three groups of people pushing towards
the ability to change this:

1. At least some PVP advocates have requested (or demanded) that
   package authors who will not follow the PVP do not upload their
   packages to Hackage. This is absolutely contradicted by the
   official guidelines of Hackage, which I've pointed out many
   times. Nonetheless, this request/demand has persisted.
2. Some opposed to the PVP do not want to upload to Hackage, basically
   because of (1). There have been many tense altercations over
   adherence to the PVP. People want to avoid this, and the easiest
   way is if they don't upload to Hackage. I know some people who
   simply do not release their code to Hackage or Stackage because of
   this. Others do so begrdugingly. But all of them would like to
   avoid Hackage for this reason.
3. Some people feel that, technically, the central repo with manually
   uploaded tarball model is outdated. They would rather see a
   workflow based on automated Git-based releases using tags or a
   release branch. This is not a social dynamic _at all_, but a desire
   to explore a different point in the technical space, which Hackage
   does not support today.

(1) has been a major pain point for me. I've requested changes to the
Hackage Trustee guidelines and Hackage rules to clarify that this
behavior (private emails demanding people not upload to Hackage,
public criticisms on individuals and companies for not following the
PVP, etc) should not be allowed. In fact, _that request_ is what
ultimately led to SLURP as far as I know. Did I demand a change with a
threat to fork? Ehh... if you want to read it that way, OK. Here's my
take: I've been told to stop using Hackage, full stop. I requested a
change in official policy to guarantee that my usage of Hackage is
allowed.

As it stands today, no such change to Hackage policy has taken
place. No final decision has been made about how I will respond to
people in groups (2) and (3). But as you can see from the sentiments
of group (3), the idea of hosting an alternative package repository to
Hackage makes no sense. Thus I can again guarantee: the most literal
fork of Hackage is something neither I nor anyone I'm speaking with
wants.

The other alternative is allowing Stackage to pull packages directly
from Git repos, in addition to pulling from Hackage. This is being
discussed as a workaround for problem (1) above. I have gone on record
in the past, and I'll go on record again now: I would rather _not_
have that situation. I would rather Hackage make it clear that it
welcomes everyone to upload its packages, and then the demands I'm
receiving to open up Stackage to alternative sources will be less
strong (though group (3) still wants to experiment for purely
technical reasons).

Am I holding a gun to someone's head? Your call. This is the most
honest version of the story I know to tell.

In sum: this is the closest to a potential fork, by allowing Git repos
to work as an alternative source to Hackage.

## Summary

I've participated in a long, private discussion with multiple people
in trying to resolve the issues referenced above. As I said: my
preference has always been for public discussions. Given how the SLURP
proposal went off, I will stand by my original claim that public
discussions are a better method. I'm sorry that the "fork" phrasing
scared so many people. To those who were truly terrified I was going
to do something nefarious: I'm sorry to keep you waiting two days in
an explanation.
