I've blogged, Tweeted, and conversed about Haskell documentation quite
a bit in the past. Following up on
[tooling issues](https://www.fpcomplete.com/blog/2015/05/thousand-user-haskell-survey),
all available evidence tells me that improving the situation for
documentation in Haskell is the next
[obstacle we need to knock down](/blog/2016/11/spreading-the-gospel-of-haskell#attacking-haskell39s-flaws).

This blog post will cover:

* Where I think the biggest value is to be had in improving Haskell
  documentation
* Status of various initiatives I've been involved in (and boy do I
  walk away red-faced from this)
* Recommendations for how others can best contribute
* A basic idea of what actions I - and others at FP Complete - have
  been taking

## Intermediate docs

In my opinion, the sore spot for Haskell overall is _intermediate_
docs. (Yes, that's vague, bear with me momentarily.) I'm going to
posit that:

* Beginners are currently well served by introductory Haskell books,
  and most recently by
  [Haskell Programming from First Principles](http://haskellbook.com/)
* Once you have a solid basis in intermediate concepts, it's much
  easier to jump into libraries and academic papers and understand
  what's going on

To me, intermediate means you already know the basics of Haskell
syntax, monads, and common typeclasses, but aren't really familiar
with any non-base libraries, concurrency, or exception handling. The
goal of intermediate documentation is to:

* Teach _which_ libraries to use, _when_ to use them, and _how_ to use them
* Give a guide on structuring Haskell programs
* Educate on important techniques, including those mentioned above, as
  well as issues around lazy evaluation and other common stumbling
  blocks

Many of us who have learned Haskell over the past many years have
probably picked up these topics sporadically. While some people will
want to plow ahead in that kind of haphazard approach, my belief is
that the vast majority of users want to be more guided through the
process. We'll get to the Haskell Syllabus and opinionated vs
unopinionated content a bit later.

## Previous efforts

It turns out, as I'm quite embarassed to admit, that I've essentially
tried reinventing the same intermediate docs concept multiple times,
first with
[MezzoHaskell](http://www.yesodweb.com/blog/2012/03/start-mezzo-haskell),
and then with
[the Commercial Haskell doc initiative](https://groups.google.com/d/msg/commercialhaskell/Ou00AvRdDTU/7BtgTUrTSCQJ). You
may also include [School of Haskell](https://www.schoolofhaskell.com/)
in that list too, but I'm going to treat it separately.

These initiatives never took off. A pessimistic view is that
Haskellers are simply uninterested in contributing to such a shared
body of intermediate-level docs. I actually believed that for a bit,
but recent activity has convinced me otherwise. I think these previous
initiatives failed due to an unsatisfactory user experience. These
initiatives required people to go to an infrequently used Github repo
to view docs, which no one was doing. A few months back, a new option
presented itself.

## haskell-lang's documentation

For those who haven't seen it, you should check out the
[libraries page](https://haskell-lang.org/libraries) and
[documentation page](https://haskell-lang.org/documentation) on the
[haskell-lang.org](https://haskell-lang.org/) site. I believe this
hits the nail on the head in many different ways:

* It directly addresses a common user question: which are the best
  libraries to use for a certain task? The libraries page gives this
  answer (though it can certainly be improved and expanded!)
* We're able to curate the documentation as a community. Providing a
  list of recommended documents on this site gives a reader more
  confidence than simply Googling and hoping the author knows what
  he/she is talking about
* The collaboration is done via pull requests on markdown files. I've
  [discussed previously](http://www.yesodweb.com/blog/2015/08/thoughts-on-documentation)
  why I think this is a far better collaboration technique than Wikis
  or other options.
* Instead of requiring all docs live within the haskell-lang.org
  repository, documents can be embedded from elsewhere. For example,
  I've
  [written a conduit tutorial](https://github.com/snoyberg/conduit#readme)
  in the conduit repository, and
  [embedded its content on haskell-lang.org](https://haskell-lang.org/library/conduit)
  via
  [a simple inclusion mechanism](https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/package-conduit.url). This
  allows authors to maintain their documentation individually, but
  provide users with a central location to find these kinds of
  documents. (I'd encourage other sites to take advantage of this
  transclusion technique, getting quality content into user hands is
  the goal!)

haskell-lang tries to host only "uncontroversial"
documentation. Documents explaining how to use a library are pretty
straightforward. Recommending libraries like bytestring, text, and
vector are all pretty well accepted. And for cases where multiple
libraries are used, we link to both.

I've merged all of the content I wrote in MezzoHaskell and the
Commercial Haskell doc initiative into haskell-lang.org where it
fit. However, there was still some more controversial content left,
such as
[exceptions best practices](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell),
which I know many people disagree with me about. Also, I'd like to be
able to tell a user looking for a solution, "yes, there are multiple
libraries around, I recommend __X__." Neither of these belong on a
community site like haskell-lang, so for those...

## More opinionated content

This is where alternative sites thrive. Since I'm collaborating with
others at FP Complete on this, and actively using this in
[training courses](https://www.fpcomplete.com/training), I've put
together a
[Haskell Syllabus page](https://www.fpcomplete.com/haskell-syllabus)
page. This is where I'll tell someone "you should do X, even though
others disagree with me." I won't enumerate the contentious decisions
here (odds are someone else will ultimately make such a list on my
behalf).

And if you disagree with this? Write a new syllabus! I think it would
be a healthy thing if we could get to the point as a community where
we could have multiple recommended, opinionated syllabuses, and link
to them with a short description for each one. This may sound at odds
with some of my previous statements, so let me clarify:

* When there's an obviously best choice, tell the user to use it
* When most users will be best with one choice, and another option is
  available, mention it as a footnote
* When multiple options are available and there's no way to know which
  the user will want, break down and give them all the information
  they need. But...
* Try to make that happen as infrequently - and as late in the
  learning process - as possible! If we could have a "you've completed
  Beginner Haskell, please choose between one of the following," and
  explain the difference between "FP Complete's course" vs (for
  example) "lens-first Haskell", that would be a good trade-off.

My thoughts on this are still evolving, and will likely change in the
future as I get more feedback from users.

## Writing style

Another big change I've made over the years is writing style. I wrote
the Yesod book in a very prose-heavy manner, focusing on explaining
details with words, and using concise, to-the-point code
examples. Many users have given me feedback to push me in a different
direction. Instead, I've recently been writing trying to write in a
very different, and thankfully easier to write, style:

* Short explanation of what the thing is I'm talking about and when
  you'd use it
* Synopsis: medium sized code snippet to give a flavor (I used this in
  the Yesd book too, and stole the idea straight from Perl docs)
* A series of increasingly complex examples, with the bare minimum
  amount of content around it to explain what's going on

I'd put this style into a hybrid of tutorial and cookbook, and think
it works well overall. I've only heard positives so far versus
previous styles, so that's encouraging. Some examples:

* The
  [http-client tutorial](https://haskell-lang.org/library/http-client)
  fits this well, since there are no deep concepts to explain
* [conduit](https://haskell-lang.org/library/conduit) has a bit more
  explanation, but is still mostly code examples
* At the other end,
  [Primitive Haskell](https://haskell-lang.org/tutorial/primitive-haskell)
  and
  [Covariance and Contravariance](https://www.fpcomplete.com/blog/2016/11/covariance-contravariance)
  and much more concept-heavy, and therefore still have a significant
  chunk of prose

I'm taking this approach because I think it's what most users
want. Some important points:

* Not all users are the same! There will almost certainly be users who
  would prefer a different style of docs. Given enough user feedback
  and manpower to write docs, it would be great to cater to all
  tastes, but it's best right now to focus on the highest demand
* API docs are still necessary, and are completely orthogonal to
  tutorials. A tutorial doesn't document each API call, an
  API-call-level explanation doesn't give enough breadth, and
  certainly users need more than just the type signatures.

## What you can do

After all of that, my recommendation on how to get involved is pretty
simple:

* Pick a library that doesn't have a good tutorial
* Write a tutorial
* Submit a
  [PR to haskell-lang](https://github.com/haskell-lang/haskell-lang#contributing-content)
  to include the content
* Alternatively: get the Markdown file included in the project's repo
  instead, and include the remote file instead

## Linking to libraries

haskell-lang has a nifty feature. If you visit
[https://haskell-lang.org/library/vector](https://haskell-lang.org/library/vector),
it will display the vector documentation it has. But if you visit a
package like
[https://haskell-lang.org/library/stm](https://haskell-lang.org/library/stm)
which doesn't (yet) have a tutorial on haskell-lang, it will
automatically redirect you to the
[Stackage package page](https://www.stackage.org/package/stm). When
giving out links to people on the internet, I recommend using the
haskell-lang.org/library/XXX link.

* When a tutorial is available, the haskell-lang page is great
* When a tutorial isn't available, the doc building on Stackage is
  still the most reliable around
* In addition, Stackage docs properly link to docs built in the same
  Stackage snapshot, making the cross linking more reliable
* When present, Stackage gives preference to the `README.md` files in
  a package, which are generally more useful than the description
  fields.

## School of Haskell

I'd be remiss in not mentioning School of Haskell here. As far as I'm
concerned, School of Haskell is great platform for an individual to
write content without any collaboration. However, for all of the cases
I'm describing here, some kind of easy collaboration (via pull
requests) is a huge win. Opening things up more with pull requests,
README.md files, and embedding content into multiple external sites
seems like the best option today (until someone comes up with
something better!).
