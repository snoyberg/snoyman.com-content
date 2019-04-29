---
title: Haskell Success Program
---

## Haskell Success Program

* Michael Snoyman
* VP of Engineering
* FP Complete webinar
* May 1, 2019

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
</div>

---

## About me

* Found Haskell about 12 years ago
* Wanted a language that kept me honest
* Invested a lot of time into making Haskell a real commercial option
* Experienced personally and through others the difficulties in commercial Haskell adoption

---

## About FP Complete

* Founded by our CEO, Aaron Contorer, about 7 years ago
* Goal: help solve the [Global Software Crisis](https://www.fpcomplete.com/blog/2012/12/solving_the_software_crisis)
* Method: more functional programming, and Haskell in particular
* Result: lots of experience using Haskell in industry

---

## Haskell Success Program

* New initiative from FP Complete
* Provide an easy onramp option for companies interested in Haskell
* Offer the most common services necessary to help get past obstacles to adoption
* Provide educational material on **why Haskell**

More information at https://haskell.fpcomplete.com/

---

## This talk is intended for

Developers/team leads who:

* Are already (mostly) convinced of Haskell
* Want to understand better the business advantages
* Interested in what FP Complete can do to help

This talk will _not_ try to sell Haskell to you if you're not already
bought in!

---

## Attractors and Obstacles

* Attractor: reason to do something
    * Skydiving is fun
    * Ice cream tastes good
    * Driving fast saves time
* Obstacles: reason _not_ to do something
    * I'm afraid of heights
    * It's not healthy
    * I might crash

This model is central to our business approach

---

## Haskell's attractors

We'll cover some more later, but in brief

* Strong type system to make code more robust
* High level of developer productivity
* Great performance-to-SLOC ratio
* Scales nicely to large teams
* Large set of libraries

---

## Haskell's obstacles

* Steep learning curve, especially for imperative/OO programmers
* Smaller job pool <=> smaller hiring pool
* Less commercial support than the big languages
* Unclear onramping
* Some gaps in tooling (especially IDEs)
* Difficult to convince others of the value of Haskell

Primary goal of the Haskell Success Program: knock down these obstacles!

---

## Haskell's 80% rule

* 80% of Haskell gives you 80% of the benefits
* Huge value-add by simply using Haskell 98
* We can sometimes forget how much power ADTs and typeclasses bring
* Overuse of more advanced features can be a deterrent to Haskell's adoption
    * Steeper learning curve
    * Abandoned project fear

---

## Learning Haskell for its own sake

* Haskell is arguably the best way to learn functional programming
* FP is well accepted in industry today
* Want to write more functional Javascript? Start by learning Haskell

---

## You need a pilot project

* Don't convert your whole codebase over to Haskell on day 1!
* Choose a small, standalone project that plays to Haskell's strengths
* Usually networked service
* Leverage concurrency
* Complex business logic
* Not a lot of integration with existing systems
* Pilot project is crucical to the learning process, otherwise the team will forget everything

---

## But why Haskell?

* Maintainable software
    * More info https://www.youtube.com/watch?v=DdR9q69se-I
* Performance- not the best, but usually a factor of 2 from the best
* Productive- get your project to market quickly
* High bug resilience (see next slide)

---

## Better than I could put it...

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Strong, static types don’t prevent all bugs. They just prevent whole classes of annoying, avoidable bugs. So you can focus on the really hard bugs: spec bugs, design bugs, UX bugs. That’s incredibly valuable!</p>&mdash; Morabijn (@morabbin) <a href="https://twitter.com/morabbin/status/1122085849502449665?ref_src=twsrc%5Etfw">April 27, 2019</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

---

## Particularly strong domains

We've found that the following are good points to lead with

* Concurrency (async/STM/explicit effects/immutability)
* Parsers (lightweight syntax/`do`-notation)
* Streaming data
* Property testing

---

## Haskell does fine

Don't claim Haskell does this better than everyone else, but it's good
enough

* Web server/client
* Unit testing (purity arguably makes this strong)
* Library coverage (though missing some domain-specific things from
  JVM/.NET/Python worlds)

---

## Where Haskell _doesn't_ shine

Acknowledge that not everything in Haskell is great

* GUI and frontend web development
* Don't start with category theory! Not needed, scares people away
* Async exceptions are a pain
* IDE support

---

## How FP Complete can help

* Training
* Consulting on pilot projects
* Set up new projects
* Recommend best practices
* Code review
* Pair programming
* Advising on hiring

---

## Be successful with Haskell!

* Happy to help however we can
* We love seeing more Haskell adoption in the world

---

## Questions?
