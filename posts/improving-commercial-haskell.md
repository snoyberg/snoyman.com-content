I passed around a version of this document for initial feedback. I'm
now proposing this publicly. I say this below, but I want to reiterate
here: I'm only interested in doing this if there's real demand and
interest, as well as others to participate. If an improved Commercial
Haskell is something with little interest, I'll drop the topic
completely.

## Premise

We started the Commercial Haskell Special Interest Group (SIG) as an
informal organization for those interested in using Haskell in a
commercial setting. So far, it has provided a few concrete activities:

* Ability to sign up as an individual or a company to be “part of” the group
* A central place for some documentation, which honestly hasn’t taken off too significantly
* A Github organization for placing some community projects, like Stack, Stackage, and others

The premise of this proposal is: can and should we do more? Are there
additional activities that the Commercial Haskel SIG should be
responsible for? Should the decision making process and meaning of
membership be clarified?

And then, most importantly, is this something that others are
interested in participating in? If this is just a group with a handful
of active members, there’s not reason to formalize things at all.

## Possible goals

First of all, I believe we should be defining clearly what the goal of
the Commercial Haskell SIG is, and some potential subgoals. As a
headline, I think the overriding goal should be:

**Increase the commercial adoption of Haskell**

I can think of some concrete subgoals which are in line with the
above.

* Encourage more serious consideration of security vulnerabilities in the Haskell ecosystem, such as:
    * Provide a Responsible Disclosure policy
    * Encourage security reviews of popular packages
    * Begin tracking vulnerabilities in the Haskell ecosystem more properly (e.g., via CVEs)
    * Prior art: https://www.reddit.com/r/haskell/comments/4uta1e/proposal_tracking_security_holes_and_major_bugs/
* Provide forums for commercial users to discuss issues and collaborate on solutions
* Put together marketing-style material for Haskell in industry
* Increase the set of documentation provided by Commercial Haskell, and probably host on commercialhaskell.com
    * One easy, concrete, and hopefully beneficial to the ecosystem: I propose to remove haskell-lang.org, and instead put its documentation on commercialhaskell.com
    * Concretely, this will hopefully help rectify a mistake I made, and provide a clear vision for why this site exists and is separate from haskell.org (providing opinionated, commercially-geared documentation, without needing to distance from haskell.org)
* Establish and encourage a Code of Conduct. This has been an ongoing discussion across the Haskell ecosystem.

## Possible problems

I’ve debated this off-and-on for a while now. Here’s a brain dump of potential hurdles:

* "Yet another committee" overhead. We have enough of these already, and they are not well loved in general.
* If we establish clear committee rules, they may be open to abuse. This would depend on organizational structure and how we determine voting occurs.
* There’s a non-trivial time and energy investment in this, and I personally don’t have a lot of either right now. This will only work if others are truly engaged.
* Maybe no one cares about any of this, and formalizing things is strictly worse than the status quo.
* Maybe there’s not alignment on what the Commercial Haskell SIG should be doing, and all we’ll be doing is creating a new public fight.
* Maybe some people who are signed up on the repo don’t believe on our goals, and will object to any clarification of direction.

## Organizational structure

Some basic ideas, I haven’t given this much thought yet.

Option A: Typical committee, a group of 7-ish people, privately choose members, no real accountability

Option B:

* Individual initiatives have a single owner or small group of cooperating owners
* Approved list of members of Commercial Haskell SIG
    * Question: how do we bootstrap that list?
    * What is the criterion for membership? Individuals? Companies?
* If there’s an objection to actions by the owner, can take a vote to override
    * Simple majority? 60%? Two-thirds?

Option C (proposed by a reviewer): A Benevolent Dictator for Life (BDFL) model

## Discussion

I've created [a Github
issue](https://github.com/commercialhaskell/commercialhaskell/issues/141)
for discussion, and will be participating there. Discussion is of
course free to occur elsewhere, but I'm going to focus my energies on
that Github issue. For real-time chat (if relevant), the
[commercialhaskell Gitter
channel](https://gitter.im/commercialhaskell/commercialhaskell) is a
great place. If there's anything sensitive people want to raise, I'm
also available for private communications.
