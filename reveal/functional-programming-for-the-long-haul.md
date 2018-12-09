---
title: Functional Programming for the Long Haul
---

## Functional Programming for the Long Haul

Michael Snoyman, VP of Engineering

<img src="/static/fpcomplete-logo.png" style="border:0;margin:0">

December 14, 2018

<img src="https://functionalconf.com/static/images/social/fuconf_twitter_card.png" style="height:150px;border:0;margin:0">

---

## My name is Michael Snoyman...

* and I'm a functional programmer
* Programming for about 25 years
* Haskell for about the last 10
* How do we choose languages, libraries, and frameworks?
* How can we do it better?
* Let's start with my history

---

## My first two "languages"

* Volunteer taught my grade school class Logowriter
* He also gave me a QuickBasic book
* Changed my life, yay!
* Choosing was easy: no choice
* Found a (Visual) C++ book, learned that
* Java became popular, learned that, used for a while

---

## My language selection process

* There wasn't one
* Not lots of learning material available
* Limited availability of tooling
* Followed "obvious" choices (Java will rule the web!)
* Decent criteria, if not great, given the constraints I had
* Then something happened...

---

## The internet

![The internet](/static/longhaul/aol.jpg)

---

## Information overload

* Learning material abundantly available
* Download compilers/IDEs/etc
* Enter: paradox of choice

---

## Some defaults still remain

* Choose the right tool for the platform, e.g.
    * Objective C or Swift a good default choice for iOS
    * Kotlin for Android
* Match the existing codebase
* Corporate coding standards

What do you do if none of that helps?

---

![Languages](/static/longhaul/languages.jpg)

---

## Non-starter: full evaluation

* Try out each language fully on a project
* Takes too much time
* Costs too much
* Deadlines need to be met
* Instead, we need some help

---

## Evaluation criteria

* We need to make a choice
* We've eliminated the easy ways to choose mentioned already
    * No choices available
    * Some obvious, default choice
* Need a smarter pruning system
* Want to come up with a short list, priority sorted

---

## Benchmarks

* We _love_ benchmarks
* Compare traffic on a review of library features post vs benchmarks
* But mostly misguided: things we benchmark usually aren't the bottleneck
* Example: speeding up HTTP protocol handling 50% likely won't make
  much difference in your application
* (We still need to make faster code as an industry)
* Why do we care so much?

---

<div style="margin-top:250px">
<h4>Benchmarks are one of the few objective criteria we use when comparing languages, libraries, and frameworks</h4>
</div>

---

## Why benchmarks (or not)

* Benchmarks can be deeply flawed
* But engineers love pointing to numbers and claiming victory
* Benchmarks are a valid evaluation criterion
* Often weighted more heavily than they should be
* Considered objective truth, but:
    * They can be misleading
    * May not be as relevant as we expect

---

## Hype

* If it's popular it must be good, right?
* Deeply flawed, but still provides _some_ signal
* Also goes by another name: reputation
* May be accurate, but may not be
* Need to know whom to trust
* Ask trusted friends/colleagues first
* Observe market trends

---

## Trust the Reddit hivemind

<img src="/static/longhaul/resf.png" style="margin:0;border:0"><br>
<img src="/static/longhaul/pcj1.png" style="margin:0;border:0"><br>
<img src="/static/longhaul/pcj2.png" style="margin:0;border:0"><br>

---

## Marketing

* We have a visceral negative reaction to hype
* Problem: it can be gamed through good marketing
* Solution: hate the game, not the player
* Game theory: everyone needs to level the playing field
* We shouldn't be afraid of doing _accurate_, _respectful_ FP marketing

---

## Where are we?

* No default or obvious choices to be made
* Benchmarks have eliminated a few non-starters
* Reputation has eliminated others
* We have a short list, priority sorted
    * Could reasonably solve our problem
    * Decent performance numbers
    * Well liked by some subset of the world you trust

---

## Onramping

* Gotta actually do the work
* Time to learn the language
* Get the tools, follow the tutorial, play with Hello World
* Compare and contrast syntax and semantics with what you know
* Write an actual demo/prototype/whatever
* Try one, get bored, try another
* Eventually, language X is the victor!

---

## Problems with approach

* Still pretty costly, unavoidable
* Easily take 1-2 months to make a decision
* Problem 1: sunk cost fallacy activate!
* Problem 2: we only know half the story

---

## Long term costs

* Benchmarks: flawed, but tell us about performance
* Initial eval: tells us initial productivity
* What about long term maintainability?
    * Bug count
    * Ability to rearchitect
    * Respond to changing requirements
    * Scalability to large teams
    * Technical debt
    * Onboarding new team members
* But what about dat hype?

---

![Sunk cost](/static/longhaul/sunkcost.png)

Those advocating the language have already fallen for the same trap

---

## Isn't this a FP conference?

* Most programmers today start imperative or OO
* Taught in schools
* Large budgets for tooling
* Lots of educational material
* __Easy onramping__
* Money for marketing == more hype

---

## We're fighting a losing battle

* Claim: highly trained Python and Haskell devs can complete task in
  the same time
* Most OO/imperative programmers won't believe that
* Someone with no FP background will _definitely_ take longer to ramp
  up
* Fighting the battle on initial eval hamstrings us
* Recent FP uptake is leveling the playing field

---

## Why this matters

* Initial development phase a small fraction of time
* Both commercial and open source work: maintenance phase is _far_
  longer
* Requirements change
* Bugs need to be fixed
* Technical debt must be paid
* Architecture must change
* Standard evaluation will not touch any of this

---

## The industry needs this

![Bug count](/static/longhaul/bugcount.jpg)

---

## Changing the conversation

* Let's talk about maintainability
* Blog about it
* Discuss with management
* Don't pass off maintenance to "the junior devs"
* Continue to make the case of performant, productive FP

---

## My belief

* Strongly typed FP great for maintainability
* Not a panacea: still subject to
    * Poorly designed APIs
    * Subtle brekages in backwards compat
    * Poorly captured invariants in types
* However, when done right, it's awesome
    * Focus on quality and maintainability
    * Still gotta test your code!

---

## My experience (anecdotal)

* Onboarded coworkers to massive Haskell projects quickly
    * Lack of mutable state
    * Data driven nature of code
    * Compiler-driven development
* Ability to refactor/rearchitect in massive ways
    * Faster feedback than unit tests
    * Attempt changes I would never dream of without types
* Relatively painless to track upstream changes

---

## FIXME concrete code examples for maintainability

---

## What can we do?

* Keep getting FP ideas out into the mainstream
    * People aren't afraid of folds anymore
    * FP is mostly seen as an advantage today
    * Learning curve is reduced
* Make it fun, easy, and exciting to learn an FP language
* Change the conversation: get people talking about long term
  maintainability
* Talk about practical benefits
* Would love to have some data to back up my claims

---

## The rest of my story

* Moved into Java web dev
* So painful that I tried PHP and _liked it_
* Eventually admitted that the long term pain was too high
* Jumped to Perl, D, tried others
* Perl 6 looks fun, what's this Pugs thing
* Pugs is written in Haskell... what's that?

---

## Learning Haskell

* I like math, let's do this!
* Docs suck, tools suck, this isn't a real language
* Played with it anyway, fell in love
* Through a lot of luck (or self delusion), found a really great language
* And years later, I'm convinced it meets the goal of maintaining large software projects

---

## Don't rely on luck!

* Others may not be stubborn like we have been
* Far too easy to make an initial guess and get locked into a bad choice
* We have a responsibility to help our industry
* Talk about the benefits of FP
* Improve the conversation
* Enjoy the conference, learn a lot!
