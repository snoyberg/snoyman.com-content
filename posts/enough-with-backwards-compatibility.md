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

Let's just cut to the chase: striving for backwards compatibility is
stupid, and we should stop doing it. I'm going to demonstrate why.

## It doesn't stop bugs

Let's take as an assumption that you're actually writing proper
changelogs for your libraries (seriously, please write proper
changelogs for your libraries). If so, there's no reason why backwards
compatibility is useful for preventing bugs.

When you are using a library, you're responsible for keeping up to
date with new versions. When a new version of any dependency comes
out, it's your sworn duty and sacred obligation to thoroughly review
the changelog and ensure that any changes that may affect you are
addressed in your code. It's just the bare minimum of good code
maintenance.

I'll leave as a given that everyone is using some version of Semantic
Versioning (SemVer), so breaking changes will never accidentally
affect their code bases.

## It's time well spent!

This may sound like a lot of time invested on a maintainers
part. While it certainly takes some effort, this is an investment in
your future, not just an expenditure. Requiring this maintenance level
from library authors is a great forcing function:

* It proves you're serious about maintaining your code. When I'm
  choosing between different libraries for a task, the library with
  regular updates to address changes in dependencies is always the
  higher quality one.
* Conversely, a lack of frequent updates tells you that you shouldn't
  trust a piece of code. Bitrot is no joke; code gets bad over time!
  Having something that forces you to change code regularly is the
  best way to avoid bitrot.
* It properly penalizes you for using too many
  dependencies. Encouraging decreased dependencies is a great way to
  avoid dependency problems!

## Failure cases

I want to take as a case example two well-known cases of well
maintained backwards compatibility, and demonstrate how destructive it
is.

### Java

Java is well known as an ecosystem that prefers stability over
elegance, and we can see that this has led to a completely dead,
non-viable platform. When generics were added to Java, they had the
choice to either break their existing containers APIs, or add a brand
new API. As we know, they made the foolish choice to introduce a brand
new API, creating clutter (we _hate_ clutter!).

But worse: it allowed old code to continue to run unmodified. How can
we trust that that code is any good if no one has been forced to
update it to newer APIs? They wasted a great opportunity for a quality
forcing function on the entire ecosystem.

### sqlite3

Once again seriously: sqlite3 is probably the most
[well designed C API](http://sqlite.org/capi3ref.html) I've ever seen,
and by far the best low-level database API I've seen. Everything seems
great with it.

But unfortunately, a few functions were misdesigned. For example, the
`sqlite3_prepare` function was designed in such a way, leading to
degraded error reporting behavior. To rectify this situation, the
author made the (misguided) decision to introduce a new API,
`sqlite3_prepare_v2`, with a different type signature, which allows
for better error reporting. (You can
[read the details yourself](http://sqlite.org/capi3ref.html#sqlite3_prepare).)

This allows existing programs to continue to compile and run
unmodified, even allowing them to upgrade to newer sqlite3 versions to
get performance and stability enhancements. What a disaster!

* There's nothing forcing programmers to use the newer, better API
* The API just _looks_ ugly. Who in the world wants a `_v2` suffix?
  Terrible!
* Someone might accidentally use the old version of the
  function. Sure, it's well documented in the API docs to use the new
  one, but unlike changelogs, no one actually reads API docs. Even in
  a language like Haskell with a deprecation mechanism it wouldn't
  matter, since everyone just ignores warnings.

Obviously, the right decision was to create a new major version of
sqlite (sqlite4), rename all of the functions, update the changelog,
and force everyone to update their codebases. Why the author didn't
see this is beyond me.

## Dynamically typed languages

Let's say you go ahead and make a breaking change to a library in a
dynamically typed language. Obviously all of your users should just
read the changelog and update their code appropriately. There's no
compiler to enforce things of course, so it's more likely that things
will fail at runtime. That's fine, if you're using a dynamically typed
language you deserve to have your code break.

## Statically typed

The story with statically typed languages is&mdash;as
always&mdash;better. There are two kinds of breaking changes we should
discuss.

### Changed type signature

Or more broadly: things that lead to a compiler error. I don't even
understand why people talk about these. The compiler tells you exactly
what you need to do. This is redundant information with the changelog!
How dense can you be! Just try to compile your code and fix things. It
couldn't be easier. I can't even right now.

### Semantic change

Sometimes a function keeps its signature but changes its behavior,
leading to code which will compile but behave differently. Sure,
that's bad, but if you're too lazy to read the changelog and
meticulously go through your codebase to see if you're affected in any
way, you shouldn't be writing software.

## Conclusion

I hope we can put this silly discussion to bed already. Break APIs!
Not only does it lead to more beautiful libraries, but the forced
breakage is good for the ecosystem. I hope the software engineering
community can finally start to take responsibility for doing proper
software maintenance.
