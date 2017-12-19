I gave a talk today at the [F(by) 2017 conference](https://fby.by/) in
Minsk, Belarus. The conference was great, I would definitely recommend
it in the future. Thank you very much to the organizers for the
opportunity to present on Haskell.

I prepared for this talk differently than I've prepared for other
talks in the past. I'm very comfortable writing up blog posts, but
have always found slide preparation difficult. This time around, I
wrote up the content in mostly-blog-post form first, and only created
the slides after that was complete. Overall, this worked very well for
me, and I'll try it again in the future. (If others want to share
their approaches to preparing talks, I'd definitely be happy to hear
them.)

As a result: I'm able to share the original write-up I did as
well. For those who saw the live talk (or the video): you may want to
skip towards the end, which covers some material that there wasn't
time for in the talk itself.

If you'd like to follow with
[the slides](https://www.snoyman.com/reveal/what-makes-haskell-unique),
they're also available.

* * *

My name is Michael Snoyman. I work at a company called FP
Complete. One of the things we do is help individuals and companies
adopt Haskell, and functional programming in general. And that leads
right in to the topic of my talk today:

**What makes Haskell unique**

Programmers today have a large number of languages to choose from when
deciding what they will learn and use in their day to day coding.  In
order to make intelligent decisions about which languages to pursue,
people need to be able to quickly learn and understand what
distinguishes one language from another.

Given that this is a functional programming conference, it's probably
no surprise to you that Haskell can be called a functional programming
language. But there are lots of languages out there that can be called
functional. Definitions vary, but let's take a particularly lax
version of functional programming: first class functions, and higher
order functions. Well, by this defintion, even a language like C
counts! You may want to limit the definition further to include
syntactic support for closures, or some other features. Regardless,
the same point remains:

**Haskell may be functional, but that doesn't make it unique**

In fact, there's a long list of features I could rattle off that could
be used to describe Haskell.

* Functional
* Statically typed
* Pure
* Lazy
* Strongly typed
* Green threads
* Native executables
* Garbage collected
* Immutability

Some of these features, like being pure and lazy, are relatively rare
in mainstream languages. Others, however, are common place. What I'm
going to claim is that not one of these features is enough to motivate
new people to Haskell&mdash;including people in this audience&mdash;to
start using it. Instead:

**It's the combination of these features that makes Haskell unique**

As an example: the intersection of purity, strong typing, and
functional programming style, for instance, lends itself to a high
level form of expression which is simultaneously easy to write, easy
to read, easy to modify, and efficient. I want to share some examples
of some code examples in Haskell that demonstrate how the language
encourages you to write code differently from other languages. And I'm
going to try to claim that this "different" style is awesome, though
it also has some downsides.

## Async I/O and Concurrency

Let's start off with a use case that's pretty popular today. Look at
this pseudocode and tell me what's wrong with it:

```
json1 := httpGet(url1)
json2 := httpGet(url2)
useJsonBodies(json1, json2)
```

Given the heading of this slide, you may have guessed it: this is
blocking code. It will tie up an entire thread waiting for the
response body from each of these requests to come back. Instead, we
should be using asynchronous I/O calls to allow more efficient usage
of system resources. One common approach is to use callbacks:

```
httpGetA(url1, |json1| =>
  httpGetA(url2, |json2| =>
    useJsonBodies(json1, json2)
  )
)
```

You may recognize this coding style as "callback hell." There are
plenty of techniques in common languages to work around that, usually
around the idea of promises or futures. And you may have heard
something about how Javascript futures are a monad, and expect me to
be talking about how Haskell does monads better. But I'm not going to
do that at all. Instead, I want to show you what the asynchronous
version of the code looks like in Haskell

```haskell
json1 <- httpGet url1
json2 <- httpGet url2
useJsonBodies json1 json2
```

This may surprise you, since this looks exactly like the blocking
pseudocode I showed above. It turns out that Haskell has a powerful
runtime system. It will automatically convert your blocking-style code
into asynchronous system calls, and automatically handle all of the
work of scheduling threads and waking them up when data is available.

This is pretty great, but it's hardly unique to Haskell. Erlang and
Go, as two popular examples, both have this as well. If we want to see
what makes Haskell different...

we have to go deeper.

### Concurrency

It's pretty lame that we need to wait for our first HTTP request to
complete before even starting our second. What we'd like to do is kick
off both requests at the same time. You may be imagining some really
hairy APIs with threads, and mutable variables, and locks. But here's
how you do this in Haskell:

```haskell
(json1, json2) <- concurrently
  (httpGet url1)
  (httpGet url2)
useJsonBodies json1 json2
```

Haskell has a green thread implementation which makes forking threads
cheap. The `async` library provides a powerful, high level interface
performing actions in parallel without bothering with the low level
aspects of locking primitives and mutable variables. And this builds
naturally on top of the async I/O system already described to be cheap
about system resource usage.

### Canceling

What we've seen already is elegant in Haskell, but it's not terribly
difficult to achieve in other languages. Let's take it to the next
level. Instead of needing both JSON response bodies, we only need one:
whichever one comes back first. In pseudocode, this might look like:

```
promise1 := httpGet(url1)
promise2 := httpGet(url2)
result := newMutex()
promise1.andThen(|json1| =>
  result.set(json1)
  promise2.cancel())
promise2.andThen(|json2| =>
  result.set(json2)
  promise1.cancel())
useJsonBody(result.get())
```

This code is tedious and error prone, but it gets the job done. As you
can probably guess, there's a simple API for this in Haskell:

```haskell
eitherJson <- race
  (httpGet url1)
  (httpGet url2)
case eitherJson of
  Left  json1 -> useJsonBody1 json1
  Right json2 -> useJsonBody2 json2
```

At first, this may seem like it's just a well designed API. But
there's quite a bit more going on under the surface. The Haskell
runtime system itself supports the idea of an asynchronous exception,
which allows us to cancel any other running thread. This feature is
vital to making `race` work.

And here's the final piece in the puzzle. All of the thread scheduing
and canceling logic I've described doesn't just apply to async I/O
calls. It works for CPU-intensive tasks as well. That means you can
fork thousands of threads, and even if one of them is busy performing
computation, other threads will not be starved. Plus, you can
interrupt these long-running computations:

```haskell
let tenSeconds = 10 * 1000 * 1000
timeout tenSeconds expensiveComputation
```

### Summary: concurrency and async I/O

**Advantages**

* Cheap threads
* Simple API
* Highly responsive

**Disadvantages**

* Complicated runtime system
* Need to be aware of async exceptions when writing code

## Immutability and purity

Most programming languages out there default to mutability: a variable
or field in a data structure can be changed at any time. Haskell is
different in two ways:

1. Values are immutable by default, and mutability must be explicitly
   indicated with a variable type
2. Mutating a mutable variable is considered a side effect, and that
   mutable is tracked by the type system

For example, the following Haskell-like code is impossible:

```haskell
let mut total = 0
    loop i =
      if i > 1000000
        then total
        else total += i; loop (i + 1)
 in loop 1
```

From pure code, we cannot create, read, or modify a mutable
variable. We also need to say what kind of mutable variable we want:

```haskell
total <- newIORef 0
let loop i =
      if i > 1000000
        then readIORef total
        else do
          modifyIORef total (+ i)
          loop (i + 1)
loop 1
```

This is a lot of ceremony for a simple algorithm. Of course, the
recommended Haskell way of doing this would be to avoid mutable
variables, and use a more natural functional style.

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

Besides pushing us towards this supposedly better functional approach,
why is immutable, pure code such a nice thing?

### Reasoning about code

You'll often hear Haskellers throw around a phrase "reasoning about
code." Personally, I think the phrase is used to mean too many
different things. But let me give you an example that I think is
accurate. Let's look at some pseudocode:

```
// scores.txt
Alice,32
Bob,55
Charlie,22

func main() {
  results := readResultsFromFile("results.txt")
  printScoreRange(results)
  print("First result was by: " + results[0].name)
}

func printScoreRange(results: Vector<TestResult>) {
  ...
}
```

If you look at the code above, what do you expect the output to be? I
think it would be reasonable to guess something like:

```
Lowest: 22
Highest: 55
First result was by: Alice
```

However, now let's throw in another piece of information: the
definition of `printScoreRange`:

```
func printScoreRange(results: Vector<TestResult>) {
  results.sortBy(|result| => result.score)
  print("Lowest: " + results[0].score)
  print("Highest: " + results[results.len() - 1].score)
}
```

Suddenly our assumptions change. We can see that this function mutates
the `results` value passed to it. If we're passing mutable references
to vectors in this made up language, then our output is going to look
more like:

```
Lowest: 22
Highest: 55
First result was by: Charlie
```

Since the original `results` value in our `main` function has been
modified. This is what I mean by hurting our ability to reason about
the code: it's no longer sufficient to look at just the `main`
function to understand what will be happening. Instead, we're required
to understand what may possibly be occurring in the rest of our
program to mutate our variables.

In Haskell, the code would instead look like:

```haskell
main :: IO ()
main = do
  results <- readResultsFromFile "results.txt"
  printScoreRange results
  putStrLn $ "First result was by: " ++ name (head results)

printScoreRange :: [TestResult] -> IO ()
printScoreRange results = do
  let results' = sortBy score results
  putStrLn $ "Lowest: " ++ show (score (head results'))
  putStrLn $ "Highest: " ++ show (score (last results'))
```

We know that it's impossible for `printScoreRange` to modify the
`results` value we have in the `main` function. Looking at only this
bit of code in `main` is sufficient to know what will happen with the
`results` value.

### Data races

Even more powerful than the single threaded case is how immutability
affects multithreaded applications. Ignoring the insanity of multiple
threads trying to output to the console at the same time, we can
easily parallelize our code:

```haskell
main :: IO ()
main = do
  results <- readResultsFromFile "results.txt"
  concurrently_ printFirstResult printScoreRange

printFirstResult results =
  putStrLn $ "First result was by: " ++ name (head results)

printScoreRange results = do
  let results' = sortBy score results
  putStrLn $ "Lowest: " ++ show (score (head results'))
  putStrLn $ "Highest: " ++ show (score (last results'))
```

There's no need to worry about concurrent accesses to data
structures. It's impossible for the other threads to alter our
data. If you do want other threads to affect your local data, you'll
need to be more explicit about it, which we'll get back to.

### Mutability when needed

One thing you may be worried about is how this affects
performance. For example, it's much more efficient to sort a vector
using mutable access instead of only pure operations. Haskell has two
tricks for that. The first is the ability to explicitly create mutable
data structures, and mutate them in place. This breaks all of the
guarantees I already mentioned, but if you need the performance, it's
available. And unlike mutable-by-default approaches, you now know
exactly which pieces of data you need to handle with care when coding
to avoid tripping yourself up.

The other approach is to create a mutable copy of the original data,
perform your mutable algorithm on it, and then freeze the new copy
into an immutable version. With sorting, this looks something like:

```haskell
sortMutable :: MutableVector a -> ST (MutableVector a)
sortMutable = ... -- normal sorting algorithm

sortImmutable :: Vector a -> Vector a
sortImmutable orig = runST $ do
  mutable <- newMutableVector (length orig)
  copyValues orig mutable
  sort mutable
  freeze mutable
```

`ST` is something we use to have temporary and local mutable
effects. Because of how it's implemented, we know that none of the
effects can be visible from outside of our function, and that for the
same input, the `sortImmutable` function will always have the same
output. While this approach requires an extra memory buffer and an
extra copy of the elements in the vector, it avoids completely the
worries of your data being changed behind your back.

### Summary: immutability and purity

**Advantages**

* Easier to reason about code
* Avoid many cases of data races
* Functions are more reliable, returning the same output for the same
  input

**Disadvantages**

* Lots of ceremony if you actually want mutation
* Some runtime performance hit for mutable algorithms

## Software Transactional Memory

Let's say you actually need to be able to mutate some values. And for
fun, let's say you want to do this from multiple threads. A common
example of this is a bank. Let's again play with some pseudocode:

```
runServer (|request| => {
  from := accounts.lookup(request.from)
  to := accounts.lookup(request.to)
  accounts.set(request.from, from - request.amt)
  accounts.set(request.to, to + request.amt)
})
```

This looks reasonable, except that if two requests come in at the same
time for the same account, we can end up with a race
condition. Consider something like this:

```
Thread 1: receive request: Alice gives $25
Thread 2: receive request: Alice receives $25
Thread 1: lookup that Alice has $50
Thread 2: lookup that Alice has $50
Thread 1: set Alice's account to $25
Thread 2: set Alice's account to $75
```

We know that we want Alice to end up with $50, but because of our data
race, Alice ends up with $75. Or, if the threads ran differently, it
could be $25. Neither of these is correct. In order to avoid this, we
would typically deal with some kind of locking:

```
runServer (|request| => {
  accounts.lock(request.from)
  accounts.lock(request.to)
  // same code as before
  accounts.unlock(request.from)
  accounts.unlock(request.to)
})
```

Unfortunately, this leads to deadlocks! Consider this scenario:

```
Thread 1: receive request: $50 from Alice to Bob
Thread 2: receive request: $50 from Bob to Alice
Thread 1: lock Alice
Thread 2: lock Bob
Thread 1: try to lock Bob, but can't, so wait
Thread 2: try to lock Alice, but can't, so wait
...
```

This kind of problem is the bane of many concurrent programs. Let me
show you another approach. As you may guess, here's some Haskell:

```haskell
runServer $ \request -> atomically $ do
  let fromVar = lookup (from request) accounts
      toVar = lookup (to request) accounts
  origFrom <- readTVar fromVar
  writeTVar fromVar (origFrom - amt request)
  origTo <- readTVar toVar
  writeTVar toVar (origTo + amt request)
```

There are helper functions to make this shorter, but I wanted to do
this the long way to prove a point. This looks like _exactly_ the kind
of race condition I described before. However, that `atomically`
function is vital here. It ensures that only a complete transaction is
ever committed. If any of the variables we touch are mutated by
another thread before our transaction is complete, all of our changes
are rolled back, and the transaction is retried. No need for explicit
locking, and therefore many less worries about data races and
deadlocks.

A `TVar` is a "transactional variable." It's an alternative to the
`IORef` that I mentioned earlier. There are other kinds of mutable
variables in Haskell, including channels and `MVar`s which are like
mutexes. This is what I meant when I said you need to be explicit
about what kind of mutation you want in Haskell.

### Purity's role

What do you think will happen with this program:

```haskell
atomically $ do
  buyBitcoins 3 -- side effects on my bank account

  modifyTVar myBitcoinCount (+ 3)
```

Here, `buyBitcoins` is going off to some exchange a buying about
$100,000 in bitcoin (or whatever ridiculous amount they're selling for
now). I said before that, if the variables are modified while running,
the transaction will be retried. It seems like this function is very
dangerous, as it may result in me going about $10,000,000 into debt
buying bitcoins!

This is where purity steps in. Inside `atomically`, you are not
allowed to perform any side effects outside of STM itself. That means
you can modify `TVar`s, but you cannot read or write files, print to the
console, fire the missiles, or place multi million dollar currency
purchases. This may feel like a limitation, but the tradeoff is that
it's perfectly safe for the runtime system to retry your transactions
as many times as it wants.

### Summary of STM

**Advantages**

* Makes concurrent data modification much easier
* Bypass many race conditions and deadlocks

**Disadvantages**

* Depends on purity to work at all
* Not really a disadvantage, you're already stuck with purity in
  Haskell
* Not really any other disadvantages, so just use it!

## Laziness

It's a little cheeky of me to get this far into a talk about unique
features of Haskell and ignore one of its most notable features:
laziness. Laziness is much more of a double-edged sword than the other
features I've talked about, and let me prove that by revisiting one of
our previous examples.

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

I didn't describe it before, but this function will sum up the numbers
from 1 to 1,000,000. There are two problems with this function:

1. There's a major performance bug in it
2. It's much more cumbersome than it should be

### Space leaks

The bane of laziness is space leaks, something you've probably heard
about if you've read at all about Haskell. To understand this, let's
look at how laziness is implemented. When you say something like:

```haskell
let foo = 1 + 2
```

`foo` doesn't actually contain `3` right now. Instead, it contains an
instruction to apply the operator `+` to the values `1` and `2`. This
kind of instruction is called a _thunk_. And as you might guess,
storing the thunk is a lot more expensive than storing a simple
integer. We'll see why this helps in a bit, but for now we just care
about why it sucks. Let's look at what happens in our `loop` function:

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

Each time we step through the loop, we have to compare `i` to the
number 1,000,000. Therefore, we are forced to evaluate it, which means
turning it into a simple integer. But we never look at the value of
`total`. Instead of storing a simple integer, which would be cheap, we
end up building a huge tree that looks like "add 1 to the result of
add 2 to the result of ... to 1,000,000." This is really bad: it uses
more memory and more CPU than we'd like.

We can work around this in Haskell by being explicit about which
values should be evaluated. There are a few ways to do this, but in
our case, the easiest is:

```haskell
let loop i !total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

All I've done is added an exclamation point in front of the `total`
argument. This is known as a bang pattern, and says "make sure this is
evaluated before running the rest of this function." The need to do
this in some cases is definitely a downside to Haskell's laziness. On
the other hand, as we'll see shortly, you often don't need to bother
if you use the right kinds of functions.

### Laziness is awesome

Let's go back to pseudocode and rewrite our summation:

```
total := 0
for(i := 1; i <= 1000000; i++) {
  total += i
}
```

Pretty simple. But now let's modify this to only sum up the even
numbers:

```
total := 0
for(i := 1; i <= 1000000; i++) {
  if (isEven(i)) {
    total += i
  }
}
```

OK, that's fine. But now, let's sum up the indices modulus 13 (for
some weird reason):

```
total := 0
for(i := 1; i <= 1000000; i++) {
  if (isEven(i)) {
    total += i % 13
  }
}
```

Each of these modifications is fine on its own, but at this point it's
getting harder to see the forest for the trees. And fortunately each
of these transformations was relatively simple. If some of the
requirements were more complicated, fitting it into the `for` loop may
be more challenging.

Let's go back to the beginning with Haskell. We saw how we could do it
with a loop, but let's see the real way to sum the numbers from 1 to
1,000,000:

```haskell
-- Bad
let loop i !total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0

-- Awesome!
sum [1..1000000]
```

We use list range syntax to create a list with one million numbers in
it. On its face, this looks terrible: we need to allocate about 8mb of
data to hold onto these integers, when this should run in constant
space. But this is exactly where laziness kicks in: instead of
allocating all of these values immediately, we allocate a thunk. Each
time we step through the list, our thunk generates one new integer and
a new thunk for the rest of the list. We're never using more than a
few machine words.

There are also other optimizations in GHC to avoid even allocating
those thunks, but that's not something I'm going to cover today.

Anyway, let's continue. We can easily tweak this to only add up the
even numbers:

```haskell
sum (filter even [1..1000000])
```

This uses the `filter` higher order function, and likewise avoids
allocating an entire list at once. And doing the silly modulus 13
trick:

```haskell
sum (map (`mod` 13) (filter even [1..1000000]))
```

Laziness is definitely a mixed bag, but combined with the functional
style of Haskell in general, it allows you to write higher level,
declarative code, while keeping great performance.

### Short circuiting for free

Lots of languages define `&&` and `||` operators which stop evaluation
early, e.g.:

```
foo() && bar()
```

`bar` is only called if `foo` returns `true`. Haskell works the same way, but these operators aren't special; they just use laziness!

```haskell
False && _ = False
True && x = x

True || _ = True
False || x = x
```

This even scales up to functions working on lists of values, such as
`and`, `or`, `all`, and `any`.

### Other downsides

There's one other downside to laziness, and a historical
artifact. Laziness means that exceptions can be hiding inside any
thunk. This is also known as partial values and partial functions. For
example, what does this mean?

```haskell
head []
```

Generally speaking, partiality is frowned upon, and you should use
total functions in Haskell.

The historical artifact is that many bad functions are still easily
available, and they should be avoided. `head` is arguably an example
of that. Another is the lazy left fold function, `foldl`. In virtually
all cases, you should replace it with a strict left fold `foldl'`.

### Summary of laziness

**Advantages**

* More composable code
* Get efficient results from combining high level functions
* Short-circuiting like `&&` and `||` is no longer a special case

**Disadvantages**

* Need to worry about space leaks
* Exceptions can be hiding in many places
* Unfortunately some bad functions like `foldl` still hanging around

__Side note__ There's a major overlap with Python generators or Rust
iterators, but laziness in Haskell is far more pervasive than these
other approaches.

## Others

Due to time constraints, I'm not going to be able to go into detail on
a bunch of other examples I wanted to talk about. Let me just throw
out some quick thoughts on them.

### Parser (and other) DSLs

* Operator overloading!
* Abstract type classes like `Applicative` and `Alternative` a natural
  fit, e.g.: `parseXMLElement <|> parseXMLText`.
* Able to reuse huge number of existing library functions,
  e.g. `optional`, `many`
* General purpose `do`-notation is great

```haskell
data Time = Time Hour Minutes Seconds (Maybe AmPm)
data AmPm = Am | Pm

parseAmPm :: Parser Time
parseAmPm = Time
  <$> decimal
  <*> (":" *> decimal)
  <*> (":" *> decimal)
  <*> optional (("AM" $> Am) <|> ("PM" $> Pm))
```

c/o [@queertypes](https://twitter.com/queertypes/status/941064338848100352)

### Advanced techniques

* Free monads
* Monad transformer stacks
* Lens, conduit, pipes, ...
* Lots of ways to do things in Haskell!
* It's a plus and a minus
* Recommendation: choose a useful subset of Haskell and its libraries,
  and define some best practices

## Conclusion

* Haskell combines a lot of uncommon features
* Very few of those features are unique
* Combining those features allows you to write code very differently
  than in other languages
* If you want readable, robust, easy to maintain code: I think it's a
  great choice
* Be aware of the sharp edges: they do exist!

## Q&A
