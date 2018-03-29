To get things started correctly, I'd like to define backwards
compatibility as the following:

> Maintaining the invariant that, amongst successive versions of a
> library or tool, software using that library or tool continues to
> build and execute with matching semantics (though perhaps different
> performance characteristics).

There is some wiggle room for defining if a change is properly
backwards compatible. If we introduce a new identifier which conflicts
with an old one, this may break a build, but most people accept this
as "backwards compatible enough." Also, fixing a bug in an
implementation may, in some cases,
[break something](https://xkcd.com/1172/). But let's ignore the subtle
cases, and instead just focus on the big picture: I released some new
library, and it changed the type signature of a function, tweaked a
data type, or significantly altered runtime behavior.

Let's just cut to the chase: every single time you break backwards
compatibility should be considered a software crime. I'm going to demonstrate
why.

## It wastes time

Let's face it: changelogs are a total waste of time. No one ever reads
them. Ever. And the only reason you're writing them is to notify
people of backwards incompatible changes. So now you've wasted the
time of two groups of people:

* The library maintainer has to write a changelog that no one is going
  to read
* Users of a library have to spend time reading the changelog that
  they're never going to read anyway

Instead, try this one simple trick: never break the API in any way. It
takes literally no time to not break things. So save yourself and your
users some time, and:

* Don't write useless changelogs
* Don't break APIs

## Freedom is Slavery

The freedom to break an API is a false freedom. In fact, you are now a slave. You're a slave to perfection. You will never be satisfied with your API. You'll always want to improve it. You'll want to make it safer, easier to use, or provide more efficient functions. If you go down this route, __you'll never be free__!

Instead, embrace the true freedom: your API will never be perfect. People will limp along with it as is. But this is far better than ever making a user change their code.

## Follow Java's example

Java avoids backwards incompatible changes. And there has literally never been a downside to this approach. Code from 20 years ago works perfectly on modern JVMs without any form of modification, and it is a pleasure to write against Java APIs. After all: who minds a little cruft if you don't have to learn new APIs ever?

Some people are concerned that new users of the language may have trouble understanding accumulated cruft. But seriously, just keep some log of things that have changed over time, which every new user must read, and you'll be golden. Worried about scaring off new people? Don't be. If they're not willing to invest in learning the history of your language or library, you don't want them anyway.

## Use prefixes and suffixes

Let's say you've written a function `theThing`. And now you've realized that the function needs to take some extra parameter. You're tempted to change the signature. But that's pure evil! Instead, follow one of these time-tested techniques:

* Call the new function, of course, `newTheThing`. The naming pattern is clear here: `newerTheThing`, `newNewerTheThing`, `newerNewerTheThing`, etc. It's totally obvious, easy to remember, and helpers users understand the work you've put in on your library over the years
* If for some reason you don't like that, you can go with the less elegant `_v2`, `_v3`, etc suffix: `newThing_v2`, `newThing_v3`, etc

After all, it's not like there's any other place where you could increment a number to indicate that there have been changes in your library.

## Compile time checking

This approach may unironically be necessary for dynamically typed languages (cough, Python 3, cough). But pretend like all languages are actually good, and have nice type systems and compile-time checking.

You may think that we're going through too much work here with maintaining backwards compatibility. After all, users will just have to build their project with the new release and then fix up compiler errors.

But let's be honest here. No one uses CI. And no one pins versions of their dependencies. If you break backwards compat, you're guaranteed to have dozens of production services just stop working tomorrow when the code fails to compile. There is literally nothing to do to stop this. Do you want to be responsible for taking down the worlds' banks just because you wanted to clean up an API? I didn't think so.

Do the moral thing, and _don't break APIs_!

## Conclusion

I think I've made my point more than well enough. There is never a justification to break an API. Ever. At all. If you do it, you are an evil, immoral, selfish person. Stop thinking of beauty. Stop thinking of elegance. Stop thinking of an easy to use API. Let the cruft accumulate, embrace the cruft, live in the cruft, and the world will be a better place.
