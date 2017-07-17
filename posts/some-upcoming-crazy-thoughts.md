I didn't mention it on my blog, but I put it on Twitter, so it's
probably not a surprise to most. About five months ago, we had a baby
boy (yay!). As you can imagine, new babies take a lot of energy,
especially when it's your first baby of four to have colic. Many
nights were spent walking Lavi around the block singing. I'm actually
pretty lucky none of the neighbors called the cops, my singing
definitely counts as disturbing the peace.

<img src="/static/lavi.jpeg" style="max-width: 100%">

Pro tip to any new parents even a little bit superstitious: never say
“We've been through all of this before, this baby can't surprise us.”

Anyway, I'm not wont to share personal anecdotes on this blog, but I
mention this because I've obviously been pretty distracted with baby
things. Fortunately, the baby is just about done with colic (just in
time to start teething of course). Between that extra energy drain
evaporating, having had lots of time to let my mind wander while
walking a crying baby, and a few other things I'll detail at the end
of this post, I've gotten to mentally explore some crazier ideas.

I've already been blogging a bit on fpcomplete.com about monad
transformers. Expect some similar things on streaming data and
polymorphism (perhaps) in the next few weeks. Also, I'll probably talk
more about exceptions, though the thoughts there are less crazy and
more reaffirming previous things.

A good question is why am I bothering with this blog post at all. I
actually drafted most of it and then decided not to publish it for
about a week. My thinking here is I don't want anyone taking my crazy
thoughts too seriously. I like to explore ideas, and I explore ideas
best by actually writing libraries and blog posts about them. In other
words, I throw things at the wall and see what sticks. I usually buy
into the idea completely for a bit to avoid second-guessing derailing
an idea, and then take a step back afterwards to see if I like it.

Besides having reduced keyboard time for the past five months, here
are some of the other stimuli leading to some of the ideas I'll be
sharing:

* I've spent considerably more effort on training. I've been doing
  documentation and tutorial writing for a while, but I've had
  multiple opportunities recently to train in a more direct
  setting. This has helped remind me of some of the newcomer
  experiences I've forgotten.

* Similar to this, my time at LambdaConf earlier this year was
  great. My conference experiences usually are either non-functional
  programming conferences where I'm the Haskell anomaly, or advanced
  functional crowds. The huge mixture of experience levels with FP and
  Haskell at LambdaConf was wonderful and eye-opening (or perhaps
  reopening).

* I've been working on a few projects where my major focus is on
  review and debugging, which forces me to focus less on making it
  easy to write code the first time, and more on writing code for
  maintainability and robustness (yeah, vague terms, don't beat me up
  over it).

* Most recently, I did a major 10-day-straight hacking fest on the
  Stack code base, after not having seriously touched it for months
  (and the parts in question for over a year). I got to play with
  major refactorings and focuses on readability and future
  extensibility.

* And in addition to all of this Haskell stuff, I've finally forced
  myself to start learning a new language for the first time in ten
  years. I went through quite the journey through programming
  languages before I hit on Haskell, and since then I've been so happy
  with it that I haven't wanted to touch anything else. But in the
  past half year, I've gotten into two languages to various extents:

    * PureScript This honestly wasn't much of a learning experience,
      since it's close enough to Haskell. I think GHCJS is a great
      project, and have enjoyed both Reflex and various React layers
      in it. But the smaller output and strict nature of PureScript
      make it something I wanted to experience for front end
      development.

    * Rust As I said on Twitter: "Rust is the first language I've
      learned in ten years (since Haskell) that both teaches new
      concepts and does stuff Haskell can't." Rust is an interesting
      language, promotes safety in a way that I like (the main reason
      I love Haskell to be honest), and has a really well designed
      community experience around it.
