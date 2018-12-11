This blog post will focus on the situation for Haskell, though the
ideas likely generalize well to other languages, or even to
non-programming disciplines. I'm simply speaking to the topic I have
the most experience with.

Many people will make claims that a certain decision needs to be made
"for new users." These recommendations can range anywhere from
sensible, to confusing, to insulting. New users are thought of
anywhere from smart people who need decent information provided, to
individuals hungry for knowledge, to idiots who don't know any better.

I've discussed this situation over time with people, and have spent
significant time personally and professionally looking for ways to
improve the situation for "new users." And I believe that, by far, the
most important trait we need to do this is _empathy_. To quote
Wikipedia:

> Empathy is the capacity to understand or feel what another person is
> experiencing from within their frame of reference

Let me start off with the most extreme example of where we, as
Haskellers, have failed historically: monad tutorials.

## The monad tutorial fallacy

Haskellers occassionally will glibly state "a monad is just a monoid
in the category of endofunctors, what's the problem." I strongly
believe the vast majority, if not all cases, of this statement are
meant to self-mock. But let's pretend for a moment that this
information was stated in this tone to a new user, who has never seen
monads, monoids, categories, or endofunctors before. This _massively_
fails the empathy test:

* The new user is unaware of these terms, and cannot understand them
  without further information
* Their frame of reference is _probably_ "I heard that monads are
  necessary to do I/O in Haskell, why is that?" The answer provides
  obfuscation in place of elucidation
* The tone, especially "just" and "what's the problem," is dismissive
  and derisive. If I'd heard a statement like that, I'd probably want
  to walk away from Haskell

Note that none of these statements require us to believe anything
negative about a new user. I've not implied that they are
unintelligent, lazy, weak-willed, or unsuitable to Haskell. My mental
model of their frame of reference is fairly straightforward here: they
have not been exposed to this material previously, they're trying to
get an answer to a specific question, and they don't want to be made
to feel inferior.

The monad tutorial fallacy in general (aka "monads are like burritos")
is a more subtle form of this. The author of such a tutorial, in their
mind, has decided that monads are somehow like burritos. This is
ignoring the frame of reference of the reader, who likely has no
reason to believe monads actually are like burritos, and cannot
understand the analogy. Sharing the analogy as if it will help, when
in reality it doesn't, can make a reader feel frustrated, confused,
and inferior, and prevent them from wanting to go further. Again, this
is _not_ a failing in the new user!

## My mental model for a new user

I've built up a mental model for how I believe a new user to Haskell
will _typically_ be thinking. This is by no means universal; many
people are exceptions. But I've found that the following traits
generally apply to someone trying out Haskell for the first time. And
making these assumptions about new users who _don't_ fit this
description perfectly doesn't usually cause any kind of major negative
impact:

* Skeptical: they don't yet know if Haskell will live up to the hype
  and actually solve their problems
* Impatient: there are dozens of languages that they could be
  investigating, and they want to quickly figure out if Haskell is one
  of the select few worth spending more time on
* Curious: few people pick Haskell as a language to look into without
  some level of curiosity about what makes this language so cool
* Smart, but distracted: I assume that, despite this curiosity,
  impatience may win out, and new users will probably be juggling a
  few other things concurrently. Maybe they're testing out other
  languages, or have some deadline at work, or who knows what.
* Eager to succeed: most people don't want to fail. They want their
  efforts to pay off and be rewarded.

There are others attributes too, most of which I probably haven't
fully identified even to myself. But this is a decent starting point.

### Exceptions

Regarding exceptions to this rule: some people are _not_
skeptical. Maybe they've been a Scala developer for years, have seen
Haskell at conferences for a long time, and are totally bought into
the language before they even start. Great! Treating them as skeptical
initially may involve giving them some cool motivating cases for
Haskell (my personal favorite being something like STM and the
`Concurrently` type). Providing this extra information can, at worst,
result in them telling me "hey, I get it, can we just skip to the part
where you teach me something?"

The same applies with impatient. I optimize my Haskell teaching to get
something working quickly so that people know the investment has a
good chance of paying off. But maybe someone has decided that, come
hell or high water, they're going to spend the next 2 weeks learning
all the ins and outs of Haskell. They want to learn the Lambda
calculus from the ground up, understand some type theory, and then
write Hello World. Great! In those cases, they can easily skip my
"concurrent HTTP downloads in 5 minutes" tutorial. The other way
around&mdash;skipping the "the Lambda calculus for math
majors"&mdash;happens less frequently.

As a side note, and just to prove the point: my wife has started
learning Haskell (though our home renovations have recently gotten in
the way of that). She fulfills _neither_ of these criteria: she's
watched me using Haskell for 10 years, and isn't at all skeptical of
the language. And she's learning the language without any specific
impatience to get things working. To my knowledge, my standard mental
modeling and new user onboarding wouldn't have negatively impacted her
acquisition of Haskell, despite this mismatch..

## Practical outcomes

Alright, so given that I've built up this mental model, how does this
affect how I try to onboard new users?

1. Ensure there's a clear start location to point them to
2. Reduce the number of choices they need to make to get started
3. Include a compelling example from the beginning that will get them interested
4. Include exercises they can work on to keep that curiosity high, but
   don't make them too complicated at first
5. Provide links to go "off the beaten track" if someone has
   drastically different goals than provided

## New users are different!

This is vital to keep in mind. When I started using Haskell, Hackage
had barely started, `cabal-install` didn't really exist at all, there
were barely any libraries worth using, and there was almost no
educational material. I was absolutely a "new user," but I didn't fit
the mental model described above at all. Making any assumption about
what other new users are willing to go through based on my own
experience would be wrong.

## What are our goals?

As I've [recently
stated](https://www.snoyman.com/blog/2018/11/stackage-history-philosophy-future),
my goal is to "increase the adoption of Haskell." For me, it's vital
to improve the new user experience significantly, and this drives a
lot of what I work on in Haskell. Not everyone shares that goal, and
that's fine. But identifying these differences can help us within the
Haskell community to have more constructive discussions about how to
proceed.

One example of such a discussion (that I was not really involved in)
was the Foldable Traversable Proposal (FTP) debate. There was a lot of
discussion about "new users." I strongly believe that different
parties had wildly different ideas of who these new users were, and
what their frame of reference was. If we had the language and
perspective to openly discuss these differences, I think we could have
had a more meaningful discussion.

The same applies to many discussions I _am_ involved in, such as
tooling. You can trace many of the more opinionated pieces of Stack to
the mental model I've described above. For example, on the impatient
side, Stack is optimized for a use case of impatience: you download a
Stack executable, and it will automatically handle all of the other
tooling installation you need. It may not do it in exactly the way
every user wants (e.g. shared vs user-local GHC installations), but
that's not the goal.

Similarly, it's optimized for skeptical users. From personal
experience, running into dependency hell out of the gate is a major
turnoff for skeptical users. Ensuring that the default of the tool is
to use pretested Stackage snapshots knocks down this major obstacle to
skeptical users. This also plays into the "smart, but distracted:"
from experience, new users won't spend time reading all of the
detailed instructions you put on your site. They're impatient to get
started quickly, distracted by 15 other tabs, and will become
frustrated when line 27 of your instructions would have fixed the
problem they run into, but they just happened to miss it.
