---
title: Haskell from the Inside Out
---

## Haskell from the Inside Out

* Michael Snoyman
* VP of Engineering, FP Complete
* FLIP, Tel Aviv, Israel
* July 23, 2018

<div style="text-align:center">
<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>
</div>

---

## Get the code!

* This is a workshop, right?
* Get the code and tools!
* `git clone https://github.com/snoyberg/haskell-inside-out`
* Follow instructions in README.md
* Kick it off now, may take some time to download

----

## Format

* This is _not_ a lecture or talk
* This is an interactive workshop
* I'm going to ask questions
* There will be exercises to play with
* תרגישו בבית

----

## Haskell is _weird_

* Computers are inherently imperative
* Most programming languages are imperative
* Haskell is stubbornly _not_ imperative
* Functional, pure, lazy, immutable...
* Goal today:
    * Understand where this weirdness comes from
    * See why this weirdness is really useful

----

## Purely functional

* Haskell is a purely functional programming language
* Most other weird things derive from that
* Purely functional is nice
    * Better testing
    * "Reason about code"
    * Allows some optimizations
    * Trivially create Software Transactional Memory
* Not obvious how to make this work in a programming language

----

## Diving in

* Start with just one constraint: functions are pure
* We'll define that a bit better as we go
* Let's see how this affects our ability to write normal code

---

## Pure math

We want to evaluate this arithmetic expression

```
(2 + 3) * (4 + 5)
```

1. Do it in your heads, try to note how you procssed it
2. Let's write up an answer in imperative pseudocode

----

## Imperative solution

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

* Any objections?
* Not terribly different from how the processor itself would do things

----

## Variation 1

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
y = 4 + 5;
x = 2 + 3;
z = x * y;
return z;
```

----

## Variation 2

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
z = x * y;
x = 2 + 3;
y = 4 + 5;
return z;
```

----

## Variation 3

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
x1 = 2 + 3;
x2 = 2 + 3;
x3 = 2 + 3;
x4 = 2 + 3;
y = 4 + 5;
z = x4 * y;
return z;
```

----

## Variation 4

Do these two things do the same thing?

```
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

```
w = 1 + 2;
x = 2 + 3;
y = 4 + 5;
z = x * y;
return z;
```

----

## Takeaways

* Some programs do different things internally, but externaly behave the same
* Must calculate `x` and `y` before `z`
* Can calculate other, irrelevant things like `w`
* Can recalculate `x` as many times as desired

Let's do something similar...

---

## Say hi

* Get a name from the user
* Print the name back out
* We'll use imperative pseudocode again

----

## Basic solution

```
print("What's your name?");
str = getString();
print(str);
```

----

## Variation 1

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
str = getString();
print("What's your name?");
print(str);
```

----

## Variation 2

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print(str);
print("What's your name?");
str = getString();
```

----

## Variation 3

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print("What's your name?");
str1 = getString();
str2 = getString();
str3 = getString();
str4 = getString();
print(str4);
```

----

## Variation 4

Do these two things do the same thing?

```
print("What's your name?");
str = getString();
print(str);
```

```
print("What's up?");
print("What's your name?");
str = getString();
print(str);
```

----

## Takeaways

* Must call `getString()` and set `str` before running `print(str)`
* Cannot run other, irrelevant things like `print("Whats' up?")`
* Cannot run `getString()` multiple times

Compare to our previous takeaways:

* Must calculate `x` and `y` before `z`
* Can calculate other, irrelevant things like `w`
* Can recalculate `x` as many times as desired

What gives?

---

## Discussion

* What's the difference between arithmetic and input/output
* What's the result of running `2 + 3`?
* What's the result of running `getString()`?
* What's the result of running `print("What's your name?")`?

----

## What's a function?

* Maps input to output
* What are the input and output to the following:
    * Plus function `+`
    * `getString`
    * `print`
    * `rollDie`

----

## Results of evaluating

* Only result from evaluating `2 + 3`: the number 5
* Two results from evaluating `getString()`
    * Some I/O (a prompt to the user)
    * A string value
* Who cares if we have the number 5 multiple times? We ignore the
  unneeded ones!
* But we can't ignore the extra I/O from `getString`

----

## Back to order of evaluation

* Does it matter if we evaluate `2 + 3 = 5` before `4 + 5 = 9`?
    * __No!__
* Does it matter if we print `What's your name` before prompting for a
  name?
    * __Yes!__
* Same rules seem to apply to repeated evaluation and reordered
  evaluation

----

## Focus on math

`(2 + 3) * (4 + 5)`

* Evaluate each subexpression as many times as desired
* Rearrange order of evaluation for subexpressions as desired
* _Must_ evaluate subexpressions before full expression
* Time for some terminology
    * Pure function
    * Data dependency

----

## Pure function

* A function in the mathematical sense
* Deterministic output for given input
* Cannot observe that the function has been run besides having a
  result
    * E.g., no I/O
    * Little bit of a lie: we know the CPU got hotter :)
* Different from what we call "functions" in most programming languages!

----

## Data dependency

* Impossible to figure out `x * 2` without knowing `x`
* We have a _data dependency_ on `x`
* Challenge: evaluate `(3 + 4) * 2`, but don't figure out `3 + 4`

---

## Can we program purely functionally?

* We already saw that this fell apart for `print` and `getString`
* Why? They aren't purely functional!
* Extra result with no data dependency to force order of evaluation
* But pure functions sound so cool!

----

## The state of the world

* `print` affects the state of the world (changes the console)
* `getString` is affected by the state of the world (user input)
* Can we somehow represent that concept in a "purely functional" way?
* Can we create some data dependency out of this?

----

## Fake it

Imagine this crazy rewrite:

```
fn main(iostate1):
  iostate2 = print("What's your name?", iostate1)
  (iostate3, str) = getString(iostate2)
  iostate4 = print(str, iostate3)
  return iostate4
```

* Cannot try to put `getString` before first print, because...
* We created a data dependency!
* Various `iostate` values force an order of evaluation

---

## Exercise 1

* Go into the exercises directory
* Run `stack build`
* Run `stack runghc ex1-fake-it.hs`
* Make it run :)

```haskell
getString :: IOState -> (IOState, String)
print     :: String -> IOState -> IOState
```

* `getString` takes one argument, returns two values
* `print` takes two arguments, returns one value
* Underscore == "fill in the blank"

----

## How was that?

* It works, but it's ugly
* We're making our life really difficult
* Can we do better? Yes
* Let's make a _pattern_

----

## Type aliases

Haskell lets us create type aliases with variables

```haskell
getString :: IOState -> (IOState, String)
print     :: String -> IOState -> IOState
```

Include "dummy" value in `print`:

```haskell
getString :: IOState -> (IOState, String)
print     :: String -> IOState -> (IOState, ())
```

And now:

```haskell
type Action a = IOState -> (IOState, a)
getString :: Action String
print     :: String -> Action ()
```

A bit easier to see what's happening

---

## Exercise 2

* Convert our previous example to the new `Action` type alias
* Use `stack runghc ex2-fake-it-action.hs`

----

## Functions!

* Let's do some code reuse

```haskell
promptString :: String -> Action String
promptString msg io1 =
  let (io2, ()) = print msg io1
      (io3, str) = getString io2
   in (io3, str)
```

Which brings us to...

---

## Exercise 3

* Let's build some helper functions
* You'll need:

```haskell
showInt :: Int -> String
readInt :: String -> Int -- why is this crazy?
```

Make `ex3-prompt-int.hs` work

----

## Problem

* Lots of ceremony to just convert a `String` to an `Int`
* Can we generalize? Of course!

---

## Exercise 4

* Fix the implementation of `mapAction` in `ex4-map-action.hs`

```haskell
mapAction :: (a -> b) -> Action a -> Action b
mapAction f action io1 =

promptInt :: String -> Action Int
promptInt msg = mapAction readInt (promptString msg)
```

---

## Exercise 5

* We'll do both name and age, with helper functions!
* Fix `ex5-name-and-age.hs`

---

## Exercise 6

* Lots of effort perform two actions
* Let's write a helper function!
* `ex6-do-both.hs`

```haskell
inner :: Action ()
inner = doBoth echoName echoAge

doBoth :: Action () -> Action () -> Action ()
doBoth action1 action2 io1 =
```

Challenge: generalize the type signature for `doBoth`

---

## Exercise 7

* Implementation of `echoName` and `echoAge` is tedious
* Let's make it less tedious!
* `ex7-and-then.hs`

```haskell
echoName :: Action ()
echoName = andThen
  (promptString "What's your name?")
  print

echoAge :: Action ()
echoAge = andThen
  (promptInt "What's your age?")
  printInt

printInt :: Int -> Action ()
andThen :: Action a -> (a -> Action b) -> Action b
```

---

## Abolish `IOState`

* Let's face it: `IOState` is a pain
* Tedious to write code
* Throwing the hack in our face all the time
* Can accidentally reuse previous states
* We have enough machinery to stop using it directly

----

## Our helper functions

```haskell
mapAction :: (a -> b) -> Action a -> Action b
doBoth :: Action a -> Action b -> Action b
andThen :: Action a -> (a -> Action b) -> Action b
```

Looks like we can do everything we need with these.

----

## Hide the `IOState`

* `Action` is just a type alias right now
* Let's make it a "newtype" to hide its implementation

```haskell
newtype Action a = Action (IOState -> (IOState, a))
```

* Hide the `Action` "data constructor"
* Now we can't play with the `IOState` values directly

----

## Fake exercise 8

* Nothing to do
* Look at `ex8-newtype.hs` to see the difference
* Tada!

---

## You just learned monads

* Sorry, forgot to mention that before
* `mapAction` is "functor map"
* `doBoth` is "applicative next"
* `andThen` is "monadic bind"
* We invented these concepts automatically to meet two constraints
    * Purely functional
    * Not painful
* (Also, I kind of intended to get there)

----

## Real monads

* Using the real monad machinery in Haskell is nicer
* You get `do`-notation
* Let's see one final exercise: rewriting to real monads
* Open `ex9-enough-already.hs`

----

## Takeaways

* Monads are a natural way to sequence actions in a purely functional
  language
* However, monads are more general than that!
* We can use the same machinery for other kinds of things
    * This may not have a value (`Maybe`)
    * This carries some extra data (`State`)
* Ultimately, `do`-notation gives us a similar imperative feel, but
  based on purity
* Pure functions provide extra power

---

## Exercise 10

Remember this thingy?

```
x1 = 2 + 3;
x2 = 2 + 3;
x3 = 2 + 3;
x4 = 2 + 3;
y = 4 + 5;
z = x4 * y;
return z;
```

Rewrite it in Haskell! `ex10-extra-computation.hs`

----

## Question

* How many times did your processor calculate `2 + 3`?
* Not easy to prove it
* Let's try something else

---

## Exercise 11

Predict the output of this program `ex11-error.hs`

```haskell
main = print inner

inner :: Int
inner =
  let w = error "this is not needed!"
      x = 2 + 3
      y = 4 + 5
   in x * y
```

----

## `w` is not needed

* No data dependency on `w`
* In Haskell, we say that the value is never _forced_
* Therefore: the error is never thrown

----

## Similarly...

What does this mean?

```haskell
main = print inner

inner :: Int
inner =
  let w = print "Hello World!"
      x = 2 + 3
      y = 4 + 5
   in x * y
```

----

## Action vs pure value

* Haskell distinguishes betweens _actions_ and _pure values_
* To perform an action, we have to _sequence it_ (e.g., `do`-notation)
* To evaluate a value, we have to _force it_
* This is a natural outcome of purity
* Values which aren't forced are _thunks_, which is _laziness_

---

## Exercise 12

What's the output of this program `ex12-lazy-and.hs`

```haskell
myAnd :: Bool -> Bool -> Bool
myAnd True x = x
myAnd False _ = False

main :: IO ()
main = do
  print (myAnd (2 > 1) (3 > 2))
  print (myAnd (2 > 1) (1 > 2))
  print (myAnd (1 > 2) (2 > 1))
  print (myAnd (1 > 2) (2 > undefined))
  print (myAnd (2 > 1) (2 > undefined))
```

----

## Short circuiting for free

* Most languages: `&&` and `||` are special-cased short circuiting
* In Haskell: _everything_ can short circuit like that
* Does require thinking a bit about "is this evaluated here"

---

## Immutability

* Not going into detail here
* Have a rule: function must always return same output for same input
* Consider this function:

```haskell
f :: Int -> Int
f _ = x

x :: Int
x = 5
```

* What happens if `x` can be changed?
* QED Haskell variables must be immutable by default
* Mutable _does_ exist, but we won't get into it today

---

## Further info

* What Makes Haskell Unique [Slides](https://www.snoyman.com/reveal/what-makes-haskell-unique) [Video](https://www.youtube.com/watch?v=DebDaiYev2M&t=1s)
* [Get Started](https://haskell-lang.org/get-started)
* [FP Complete Haskell Syllabus](https://www.fpcomplete.com/haskell-syllabus)
* [Functors, Applicatives, and Monads](https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads)
* [All About Strictness](https://www.fpcomplete.com/blog/2017/09/all-about-strictness) (advanced)
