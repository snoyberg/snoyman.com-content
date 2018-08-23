# Kids Coding, Part 1

I've been wanting to give my eldest child (Eliezer, now 10) a chance
to learn to code for about a year now, with little success. My second
child (Gavriella, now 8) is also ready to start learning some
coding. "Ready" to me means "they have a decent enough grasp of
written English." (Yakov, my 6 year old, also wants in, but
unfortunately is going to have to wait a bit.)

I know how I learned to code: sitting at a DOS prompt since the age of
2 and reading a massive "Teach Yourself QBasic in 21 Days" book. For
various reasons, this doesn't seem to apply to the next
generation. I've looked into other modern approaches, including the
graphical programming environments. My kids enjoyed some of this, but
this week told me that "it's fun, but we're not learning anything."

Previously, I tried teaching Eliezer some Haskell by [writing up
lessons for
him](https://github.com/snoyberg/magical-guide-haskell). Whether
because of my writing or his interest, it didn't work at the time. I
decided to not go down this path again, and an hour ago sat down with
Eliezer and Gavriella for their first lesson. I'm winging this
completely, but here's the approach I'm taking:

* Keep the computer in front of me and show them things
* See what confuses them, what interests them, and follow that
* Give them one or two exercises at the end to reinforce the ideas
* Keep the lessons short to keep their interest
* Write up a blog post right afterward to record what happened, so I
  can learn for the future. After all, Yakov and Lavi (the baby, aka
  [#YoungestHaskeller](https://twitter.com/search?f=tweets&q=%23youngesthaskeller&src=typd))
  need to learn too

This is the first of these summary blog posts. Caveats:

* This will likely be incomplete
* The material is very much geared towards my kids and probably won't
  generalize
* I have no idea how long or frequently I'll be doing this.

I considered making these private notes for myself instead, but
thought some others may be interested, so I'm posting publicly.

Without further ado!

## Python portion

* Previous experience with Haskell made Eliezer think it was too hard
  for him, so we decided to learn Python instead. He has some
  experience with that from Code Combat.
* I started at the REPL and demonstrated printing strings. We made
  some typical jokes (yes, they were fart jokes) in text form, which
  made the kids giggle. Upshot: they learned about strings, and stayed
  interested.
* I demonstrated that basic arithmetic works at the REPL.
* I opened up a `hello-world.py` file and demonstrated you could write
  the same stuff from the REPL in a file.

This was the point where Eliezer commented that it looked a lot like
the Haskell he'd seen. I'm not sure what it was, but somehow it
clicked with him that whatever scared him off of Haskell previously
wasn't a real issue. We decided together to switch over to learning
Haskell instead, which I'm quite happy about (more because I know the
language better than anything else).

## Haskell portion

* Did the same `print`, string, and arithmetic stuff at the GHCi
  prompt and in a file
* Asked them what `2 + 3 + 4` was, they got that
* Asked them what `2 * 3 + 4` was, they got that too
* Then `2 + 3 * 4`, and I was surprised to find out that they knew
  about order of operations already. Yay school system.
* They mentioned parentheses, and I told them that would be the same
  way to do things in programming, and showed them `(2 + 3) * 4`.
* Next we talked about `print 2 + 3`. They had some inkling that this
  wouldn't work, but couldn't be sure of why.
* I then branched into the next topics...

## Types

* Types are really important in Haskell programming
* Everything has a "type" (this raised confusion and interest, as expected)
* Explained that 2 is a number, that's its type (yes, it's a bit of a
  lie, `Int`, and defaulting to `Integer`, and `Num a => a`, blah blah
  blah, this is fine for first lesson)
* Is 2 a string? No. Is 2 a dog? (giggles) No. Is 2 a missile?
  (Eliezer got to explain that concept to Gavriella.)
* `print 2 + 3` because of order of operations (just like math) really
  is `(print 2) + 3`, what does _that_ mean?
* What's the type of `print`? Confusion
* We've discussed functions a lot before, so pointed out that `print`
  is a function that takes a number (here) and returns _something
  else_. What is that _something else_?
* I introduce the idea of _actions_. Eliezer got this, Gavriella was
  more confused. So more humor to explain this.
* `fart = print "Sorry, I farted"` lots of giggling. What is the type
  of fart? Is it a number? No. Is it a dog? No. It's something that
  you do, or the computer does. That's what an action is. (Gavriella
  translated some words into Hebrew at that point, and the ideas
  clicked. Got to remember: they're learning both programming
  languages and how to learn things in English at the same time.)
* OK, you can add two numbers together. Does it make sense to add an
  action and a number together? No. So `print 2 + 3` doesn't make
  sense!
* Now, what's `print`? It's a function that takes a number and gives
  you back an action. And that's how you use functions in Haskell:
  just stick things next to each other.
* They figured out really quickly at this point that they needed
  parens to fix the program, and ended up with `print (2 + 3)`.

## Today's excercise

Note: Somewhere above, I briefly showed them that you could use `do`
notation and put multiple `print` calls on different lines. No
confusion from this at all. Relevant to the exercise:

* Write a program that print "2 + 3 + 4", first as a string, and then
  as a number.

I started them off with:

```haskell
main = do
  | -- prompt started here
```

They both got the answer, one of them with a bit of help:

```haskell
main = do
  print "2+3+4"
  print (2+3+4)
```

Things to note:

* They naturally left off the spaces, despite me using the spaces
  throughout my typing.
* They never questioned why `print "Hello World"` resulted in output
  that kept the double quotes. I'll have to explain at _some_ point
  about `putStrLn`, but that can come much, much later.

## To figure out

* How much shell do I teach them?
* What editor will I set them up with?

## Next lesson

I don't want to plan out what to cover next time too intricately,
because I want to experiment with them and bounce things around. I'm
thinking about showing them how to create their own functions, maybe
with lambda syntax, not sure.
