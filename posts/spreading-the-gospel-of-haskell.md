Yesterday I fired off two tweets about the state of Haskell evangelism:

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">I&#39;d hoped by now we&#39;d be out spreading the gospel of Haskell&#39;s awesome features. Instead, we&#39;re fighting about the same broken things.</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/800585540177432576">November 21, 2016</a></blockquote>
<blockquote class="twitter-tweet" data-conversation="none" data-lang="en"><p lang="en" dir="ltr">Haskell is by far the best language on the market today. It&#39;s so sad to see it not gaining traction because of unimportant details.</p>&mdash; Michael Snoyman (@snoyberg) <a href="https://twitter.com/snoyberg/status/800586069863301120">November 21, 2016</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

But simply complaining about the state of things, instead of actually
proposing a way to make things better, is a waste of 280
characters. So I'd like to expand on where I think we, the Haskell
community, can do better.

## Attacking Haskell's Flaws

As the Twitter discussion yesterday pointed out, there are undoubtedly
flaws in Haskell. Every language has flaws. Haskell is blessed to also
have some of the greatest strengths in any programming language
available today: beautiful concurrency, a powerful _and_ useful type
system, a plethora of real-world libraries, and (as of recently)
pretty good tooling and educational resources.

At FP Complete, we often talk about the __attractors__ and
__obstacles__ (thanks to our CEO, Aaron Contorer, for this great prism
to view things). Using that terminology: Haskell is chock-full of
attractors. The problem is the obstacles which prevent Haskell from
taking off. I'm going to claim that, at this point, we need to do very
little as far as making Haskell more attractive, but instead need to
collectively knock down obstacles preventing its success.

Obstacles can be a great many things, some of which you may have
categorized as "missing attractors." Let me give some examples:

* Missing IDE tooling. For some people, this is a deal-breaker, and
  will prevent them from using Haskell.
* A missing library. Again, if someone needs to access, say, MS SQL
  Server, and a library doesn't exist, this is an obstacle to
  adoption. (Yes, that person could go ahead and write the library
  him/herself. If you think that's the right response, you probably
  shouldn't be reading this blog post.)
* Lack of a tutorial/example/cookbook for a specific problem
  domain. Yes, someone could struggle through reading API docs until
  "it clicks." If that's your answer: also probably shouldn't be
  reading this post.
* Lack of support for an OS/architecture.

The important thing about obstacles is that they are not
universal. For most of us, lack of support for Haiku OS will not
prevent us from using Haskell. Those of us who have been using Haskell
for years have decided that the obstacles of bad tooling weren't
enough to deter us from the great language features. And so on.

## Prioritizing

Many people in the Haskell community have been chipping away at random
obstacles (or adding random attractors) for years now, on a hobbyist
basis. If that's all you want to do, more power to you, and
enjoy. What I'm doing here is making a call for a more concerted,
organized effort into knocking down these obstacles to Haskell
adoption.

I'd say that we can measure how high a priority an obstacle-destroying
action is based on two criteria:

* How difficult it will be to accomplish
* How big an impact it will have on Haskell adoption

I would call easy actions with big impact _low hanging fruit_, and
recommend we focus on those actions for now. In other words, while
improving GHC compile times may have a big impact, it's also difficult
to accomplish. Similarly, changing the Haskell logo from purple to
blue is easy to accomplish, but doesn't have much impact.

So my set of easy to do and big impact things entirely come down to
*spreading the word*. I would say our biggest and easiest knocked-down
obstacles are:

* Someone's never heard of Haskell
* Someone's heard of Haskell, but doesn't know why it's relevant
* Someone's well aware of Haskell, but thinks it will be hard to start
  with
* Someone's already tried Haskell and run into problems (like
  Dependency Hell), and doesn't realize we've solved them

So what does this entail? Here are my suggestions:

* Write a blog post about how you solved a problem in Haskell
* Give a talk at a conference on what problems Haskell is particularly
  good at solving (my money goes to concurrency on this)
* Put together a screencast on Haskell
* Encourage a non-Haskeller to go through the
  [Haskell Book](http://haskellbook.com/) and
  [Haskell Syllabus](https://www.fpcomplete.com/haskell-syllabus)

The intention here to show the world that Haskell is ready to help
them, and that it's easy to get started now. Many of us at FP Complete
have been putting out
[such](https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell)
[posts](https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack)
[for](https://www.fpcomplete.com/blog/2016/09/practical-haskell-simple-file-mirror-1)
a
[while](https://www.fpcomplete.com/blog/2016/08/bitrot-free-scripts). I'm
asking others to join in the fun and help give Haskell adoption a
kick-start.

One final request: if you've gotten this far, odds are you agree that
we need to encourage users to take the most-likely-to-succeed route to
Haskell, be that with tooling, training, library installation, or
library selection. We've put a lot of effort into making
[haskell-lang.org](https://haskell-lang.org) the destination for that
goal. Hopefully haskell.org can converge on this goal in the future,
but for now it's very likely to just present another obstacle. When
you tell people to get started with Haskell, I strongly recommend
linking to
[https://haskell-lang.org/get-started](https://haskell-lang.org/get-started).
