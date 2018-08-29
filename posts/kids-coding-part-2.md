I didn't expect to be writing the second blog post only 12 hours after the
first. But when the kids came downstairs this morning, they were unbelievably
excited to do more coding. So here we are!

Eliezer and I discussed using more visual tooling (like Code World or Jupiter)
for doing the learning, and I showed him what it looked like. It seems he's got
quite a bit of his father's preferences, and wanted to stick to plain text for
now. We'll probably circle back to that kind of stuff after they get the
basics. It will also give me a chance to get more comfortable with those
offerings. (Thanks all for the recommendations on Reddit.)

One final note. I'm extremely happy that we went with Haskell after today's
lessons. Concepts like variable replacement which are natural in Haskell were
great teaching tools. I obviously don't have enough data to prove this yet, but
I'm more strongly believing the long-held theory that Haskell is easier for
brand new programmers than those familiar with imperative programming.

## Coding environment

I moved us over to VSCode, with only syntax highlighting set up. Previously,
I'd used my emacs setup with intero, and the red squiggles let them know they'd
messed up too early. I used the docked terminal at the bottom, taught them to
save, and showed them to press "up enter" in the terminal to rerun `stack
runghc foo.hs`. Perhaps not the ideal setup, but definitely good enough.

## Variables

* Started with `main = print "hello world"`
* Explained that `main` is _defined_ as other thing
*   Expanded to

    ```haskell
    main = print hello
    hello = "hello world"
    ```
* They were not surprised at all that this just worked
* `hello = 5 + 6`, both got confused about whether it would print `11` or `5 + 6`, reminded them of strings
* How about `hello = five + six`?
* Both agreed it wouldn't work. I typed in `five = 5`, and ask if it would
  work. They agree that it won't, and I show them GHC's error message.
* "Variable not in scope." Computers talk funny, just means "I don't know what
  six is." Instilled that they'll eventually be able to understand those
  massive error messages, but not to worry about it yet.
* Exercise: five the program. Both of them got it no problem.
* I was about to demonstrate `five = 4`, and then Gavriella figured it out for
  herself! They understand that, again, "computers are dumb." Names are just
  there for our benefit, computer doesn't care.
* We do `five + six * seven` (with appropriate variable definitions), they get
  that answer, and then `(five + six) * seven`, they get that too.
* Now I define `fivePlusSix = five + six`, and change to `hello = (fivePlusSix) * seven`
  (direct replacement). They're fine with this. Yay replacement.
* Point out that the parens are now unneeded and remove them. Again, no
  problem.
* Parens just tell us "do this first", not needed for one thing

## Functions

* Type in `hello = 5 * 2`, no problem.
* How about `hello = double 5`? They figured out that it won't work cuz
  computers be dumb
* How do we define `double`? Eliezer guessed `* 2`, which is really close, but
  we're not ready for sections, and I want to teach standard function stuff
  first.
* Show them `double x = x * 2`, that `x` is a variable, and a function
  argument. They don't know algebra yet, but got this fairly quickly.
* `hello = addFive (double 5)`
* Exercise: fix the program!
* Eliezer did this one, defined `addFive`, and he started spacing things like
  me without any prompting. Cool.
* Exercise: Gavriella, write `minusTwo`
* Got it first time, ran it, answer's still 15. Why?
* We stepped through how the program is evaluated. "Let's look at `main`, oh, I
  got an action! I need to print `hello`, what's that? Oh, it's `addFive ...`.
  That means I gotta figure out `...`. What's `double 5`? Oh, it's `10`. So
  that's `15`. And `minusTwo` was never (terminology break, I taught them that
  it wasn't used).
* Exercise: make the answer 13. They're struggling to figure out where to use
  it. They're stumped, time to jump in.
* Stepped through all of the types involved. "double is a function that takes a
  number and gives you back a number" blah blah blah
* `minusTwo` is also a function. It needs one number. Let's try something
  wrong: `minusTwo addFive (double 5)`.  That's wrong: it means "apply
  `minusTwo` to two values" but it only needs one number.
* We need to turn `addFive (double 5)` into one thing. Ask them what we can
  use.  They played with idea of double quotes, and then they figured out the
  parens! Awesome!
* One final exercise: make the answer 16 with `plus3`
* Eliezer did that typing, so I gave Gavriella one real final one: make the
  answer 6 with `minusTen`. She did it.
* Realizing now: learning to type and navigate an editor is possibly harder for
  them than the coding itself

## Bonus

Gavriella kept playing with the code (wow, she's focused on this). She decided
she wanted to do division. That was more complicated, but she persevered. I
taught her about `div` being a function that takes 2 arguments. She didn't know
anything about remainders, and was confused that `div 15 2` worked at all. I
taught her about `divMod`, and she did great.
