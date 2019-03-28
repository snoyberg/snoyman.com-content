I assure you, this is a post about programming, it'll just take a few
paragraphs to get there.

There's a biological mechanism known as _resistance_, and it plays out
in many different systems. For example, as you habitually drink more
alcohol, you gain a tolerance, which prevents you from getting drunk
as easily. This can be called *alcohol resistance*. When you
habitually run high levels of insulin, your body becomes less
sensitive to insulin, making you *insulin resistant*. When you go into
a loud room, your ears adjust down the sound, making you *noise
resistant*. And when you change enough dirty diapers, you become
*smell resistant*.

Resistance applies not just at the biological level. Consider a car
crash. The first time your car is in a crash, it is impacted in a
major way. But after repeated crashes, the damage goes down, as your
car attains *crash resistance*. The first time a baseball goes through
a window, it shatters. But further impact between the shards and balls
causes less damage. This is *impact resistance*.

Resistance isn't a term we often use in the programming world. That's
a mistake I intend to rectify today.

Imagine you're working on a Python application. Python is a memory
managed language, so you have _never_ seen a segfault. One day, you're
testing your code, and **poof**, a wild segfault appears! You will
respond to it far more strongly than a C programmer, who has built up
a healthy memory bug resistance over the years. And as a result, you
may begin to develop some of that alcohol resistance I mentioned
above.

But I'm not here to talk about Python, because no one uses Python in
the real world. Instead, let's discuss Haskell.

In Haskell, we can use strong typing techniques to great effect, such
as:

* Avoiding 404 errors in a web application with type-safe URLs
* Ensuring proper character encoding handling by differentiating
  textual and binary data
* Making Software Transactional Memory safe by quarantining effects

The problem is that we use it _everywhere_. Like any system, this
overuse of typing builds up a resistance. When someone is insulin
sensitive, and you give them a small dose of insulin, they respond
quickly. When they are insulin resistant, that same dose produces
almost no impact. The same is true with typing.

Every single expression and subexpression in Haskell is typed. Sure,
there's type inference and the programmer doesn't have to spell it
out. But _it's still there_. The effects can still be felt. You cannot
escape the reality that, when you hit compile, an Abstract Syntax Tree
annotated with types is being generated. One of the intermediate
languages used by GHC&mdash;Core&mdash;type annotates *everything*!

The types are there, just like insulin. And just like insulin, whether
we realize it or not, our bodies and minds are slowly building up
resistance to it.

With insulin resistance, the body produces ever increasing amounts of
insulin to overcome the resistance, till the levels are so high that
they cause damage. So, too, with types: Haskellers end up using more
and more types in their code until they buckle under the strain. Their
minds break down, their tools break, runtime errors slip in,
performance suffers.

Types, like insulin, are not evil. But they need to be moderated.

There is no future for a language like Haskell. It dictates
overuse&mdash;nay, abuse&mdash;of types. It is inhumane and immoral to
subject our computers to such torture. We need to immediately reduce
our dependence on types, and then&mdash;when type sensitivity is
reestablished&mdash;carefully and moderately add them back in.

Python took this too far, by fully eliminating types. It's literally
impossible in Python to even get a type error, even at runtime. That's
absurd, and is just as bad as a body with no insulin production at
all. However, Python heads in the right direction. We need a blend of
Python's complete lack of a type system, and Haskell's ability to use
types in the right location.

Therefore, I'm announcing today the creation of a new language:
Paskell. It fortunately is an unused name, and sounds nothing like any
other programming language ever before created.

And for those who want to learn more, you can [watch this
video](https://www.youtube.com/watch?v=dQw4w9WgXcQ) where I describe
in more detail how I truly feel about types.
