---
title: What Makes Haskell Unique
---

### What Makes Haskell Unique

* Michael Snoyman
* VP of Engineering, FP Complete
* F(by) 2017

<div><img src="/static/fpcomplete-logo.png" style="border:0;margin:0"></div>

<aside class="notes">

<ul>
<li>Good morning, welcome</li>
<li>FP Complete helps people adopt Haskell</li>
<li>Need to know: <b>What makes Haskell unique</b></li>
</ul>
</aside>

---

## Why uniqueness matters

* Programmers have lots of options
* Need to know what distinguishes programming languages
* Need to understand what makes Haskell different from other languages

----

## Is Haskell functional?

* What even is a functional language?
* Lax definition
    * First class functions
	* Higher order functions
* Wait... is C functional?

<aside class="notes">
<ul>
<li>Haskell is functional</li>
<li>So are lots of others</li>
<li>Even if you include closures, still many choices</li>
</ul>
</aside>

----

__Haskell may be functional, but that doesn't make it unique__

<aside class="notes">
Lots of things could describe Haskell
</aside>

----

## Let's Describe Haskell

* Functional
* Statically typed
* Pure
* Lazy
* Strongly typed
* Green threads
* Native executables
* Garbage collected
* Immutability

<aside class="notes">
<ul>
<li>Some features are rare: pure and lazy</li>
<li>Some are common</li>
<li>No one feature is enough to motivate using Haskell</li>
</ul>
</aside>

----

__It's the combination of these features that makes Haskell unique__

* Example: purity + strong typing + functional style leads to:
    * Easy to write
	* Easy to read
	* Easy to modify
	* Efficient
* We'll get to this later
* Now: lots of examples!
* Different here is usually better, but some downsides

---

## Async I/O and Concurrency

What's wrong with this?

```
json1 := httpGet(url1)
json2 := httpGet(url2)
useJsonBodies(json1, json2)
```

* Hint: it's in the title of this slide
* Ties up an entire system thread on blocking I/O calls
* We want to be more efficient with resources, so...

----

## Callbacks

```
httpGetA(url1, |json1| =>
  httpGetA(url2, |json2| =>
    useJsonBodies(json1, json2)
  )
)
```

* Aka "callback hell"
* Lots of techniques to work around it, e.g. promises/futures
* "Oh, promises form a monad!" Not even going there today :)

----

## Asynchronous Haskell version

```haskell
json1 <- httpGet url1
json2 <- httpGet url2
useJsonBodies json1 json2
```

* But that looks just like the blocking code! Exactly
* Runtime converts to async system calls
* Runtime schedules threads
    * Sleeps when waiting for data
	* Wake them up when data is available
* Not only Haskell: Erlang and Go do this too
    * Therefore....

----

<img src="/static/unique/deeper.jpg" style="width:100%">

----

## Concurrency

* Why wait for `url1` before starting `url2`?
* Need to fork threads, write to mutable variables, do some locking
* Or be awesome

<div class="fragment">

<pre><code class="haskell">(json1, json2) <- concurrently
  (httpGet url1)
  (httpGet url2)
useJsonBodies json1 json2</code></pre>

<ul>
<li>Cheap green thread implementation</li>
<li>Wonderful `async` library</li>
<li>Builds on the async I/O system</li>
</ul>

</div>

<aside class="notes">
So far: elegant in Haskell, but not terribly difficult in other languages.
</aside>

----

## Canceling

* We only want one of the responses
* Take whichever comes first

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

----

## Canceling in Haskell

```haskell
eitherJson <- race
  (httpGet url1)
  (httpGet url2)
case eitherJson of
  Left  json1 -> useJsonBody1 json1
  Right json2 -> useJsonBody2 json2
```

* More than just a well designed API
* Depends on asynchronous exceptions
* Cancel any other running thread

----

## Not just about I/O

* Thread scheduling, sleeping, killing works for CPU bound tasks too!
* Don't need to worry about a heavy computation starving other threads
* No need to offload your heavy tasks to a different microservice, do
  it all in Haskell

```haskell
let tenSeconds = 10 * 1000 * 1000
timeout tenSeconds expensiveComputation
```

----

## Summary: concurrency and async I/O

<div style="text-align:left">

<h3>Advantages</h3>

<ul>
<li>Cheap threads</li>
<li>Simple API</li>
<li>Highly responsive</li>
</ul>

<h3>Disadvantages</h3>

<ul>
<li>Complicated runtime system</li>
<li>Need to be aware of async exceptions when writing code</li>
</ul>

</div>

---

## Immutability and purity

* Most languages default to mutable values
* Haskell differs in two ways:
    * Immutable by default, explicit kind of mutability
	* Mutating is an effect, tracked by the type system

----

## Mutable Haskell

Impossible

```haskell
let mut total = 0
    loop i =
      if i > 1000000
        then total
        else total += i; loop (i + 1)
 in loop 1
```

Real and tedious

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

<aside class="notes">

<ul>
<li>In pure code, we cannot create, read, or modify a mutable variable</li>
<li>Have to use non-pure code</li>
<li>Lots of ceremony for something simple, so don't do that</li>
</ul>

</aside>

----

## Better Haskell

Recursive and immutable, much better!

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

But why does this matter?

----

## Reasoning about code

Guess the output

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

<aside class="notes">

<ul>
<li>Personally think the phrase "reasoning about code" is overused, but here's a concrete example.</li>
<li><i>Describe slide</i></li>
<li>What's the expected output? Reasonable to guess...</li>
</ul>

</aside>

----

## Expected output

```
Lowest: 22
Highest: 55
First result was by: Alice
```

<aside class="notes">But let's see the definition of <code>printScoreRange</code></aside>

----

## What's in printScoreRange?

```
func printScoreRange(results: Vector<TestResult>) {
  results.sortBy(|result| => result.score)
  print("Lowest: " + results[0].score)
  print("Highest: " + results[results.len() - 1].score)
}
```

<div class="fragment">

Actual output:

<pre>Lowest: 22
Highest: 55
First result was by: Charlie</pre>

Non-local changes broke our guessed result
</div>

<aside class="notes">Our assumptions changed because of mutation</aside>

----

<img src="/static/unique/doh.gif">

<aside class="notes">

<ul>
<li><code>results</code> from <code>main</code> has been modified</li>
<li>Can't just look at <code>main</code> to understand what will happen</li>
<li>Need to be aware of mutation happening in the rest of the program</li>
</ul>

</aside>


----

## Do it in Haskell

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

* Impossible for `printScoreRange` to modify results
* `printScoreRange` sorts into a local copy
* Have to think about less when writing `main`

----

## Data races

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

* Concurrent data accesses? No problem!
* Concurrent data writes? Impossible!
* We'll come back to mutable, multithreaded data

<aside class="notes">Multithreaded cases is more interesting. We can easily parallelize our previous code.</aside>

----

## Mutability when needed

* In place, mutable algorithms can be much faster
* Example: sorting a vector with only pure transformations is __slow__
* Haskell's answers:
    1. You can still have mutable data structures if you want them,
       but they're __explicit__
	2. Temporary mutable copy, then freeze it

<aside class="notes">Option 1 breaks our guarantees. But still better than other languages: know exactly which data to look at</aside>

----

## Freeze!

```haskell
sortMutable :: MutableVector a -> ST (MutableVector a)
sortMutable = ... -- normal sorting algorithm

sortImmutable :: Vector a -> Vector a
sortImmutable orig = runST $ do
  mutable <- newMutableVector (length orig)
  copyValues orig mutable
  sortMutable mutable
  freeze mutable
```

* `ST` is for temporary, local mutability
* Cannot be affected by the outside world, and cannot affect it
* Keeps functional guarantee: same input ==> same output

----

## Summary: immutability and purity

<div style="text-align:left">

<b>Advantages</b>

<ul>
<li>Easier to reason about code</li>
<li>Avoid many cases of data races</li>
<li>Functions are more reliable, returning the same output for the same input</li>
</ul>

<b>Disadvantages</b>

<ul>
<li>Lots of ceremony if you actually want mutation</li>
<li>Some runtime performance hit for mutable algorithms</li>
</ul>

</div>

----

## Concurrent Mutation

What's wrong with this code?

```
runServer (|request| => {
  from := accounts.lookup(request.from)
  to := accounts.lookup(request.to)
  accounts.set(request.from, from - request.amt)
  accounts.set(request.to, to + request.amt)
})
```

Looks reasonable, but...

```
Thread 1: receive request: Alice gives $10
Thread 2: receive request: Alice receives $10
Thread 1: lookup that Alice has $50
Thread 2: lookup that Alice has $50
Thread 1: set Alice's account to $40
Thread 2: set Alice's account to $60
```

NOTE:

* What if you actually need to mutate values, and from multiple threads?
* *Describe slide*
* Alice ends up with either $40 or $60 instead of $50

----

## Locking

```
runServer (|request| => {
  accounts.lock(request.from)
  accounts.lock(request.to)
  // same code as before
  accounts.unlock(request.from)
  accounts.unlock(request.to)
})
```

```
Thread 1: receive request: $50 from Alice to Bob
Thread 2: receive request: $50 from Bob to Alice
Thread 1: lock Alice
Thread 2: lock Bob
Thread 1: try to lock Bob, but can't, so wait
Thread 2: try to lock Alice, but can't, so wait
```

__Deadlock!__

NOTE: Typical solution to this is to use locking, but it leads to other problems

----

## Software Transactional Memory

```haskell
runServer $ \request -> atomically $ do
  let fromVar = lookup (from request) accounts
      toVar = lookup (to request) accounts
  origFrom <- readTVar fromVar
  writeTVar fromVar (origFrom - amt request)
  origTo <- readTVar toVar
  writeTVar toVar (origTo + amt request)
```

* Looks like it has a race condition
* But STM ensures transactions are atomic
* No explicit locking required
* `TVar` is an example of explicit mutation
* Alternatives: `IORef`, `MVar`

NOTE:

* There are helper functions to make this shorter
* Want to make a point with the longer code
* STM will automatically retry when needed

----

## The role of purity

STM retries if a transaction isn't atomic. How many Bitcoins will I
buy?

```haskell
atomically $ do
  buyBitcoins 3 -- side effects on my bank account

  modifyTVar myBitcoinCount (+ 3)
```

* Trick question! Code doesn't compile
* `atomically` only allows side effects on `TVar`s
* Other side effects (like my bank account) are disallowed
* Safe for runtime to retry thanks to purity

NOTE:

* `buyBitcoins` needs to go to an exchange and spend $100,000
* Due to retry, this code could spend $10m
* This is where purity steps in

----

## Summary of STM

<div style="text-align:left">

<h3>Advantages</h3>

<ul>
<li>Makes concurrent data modification much easier</li>
<li>Bypass many race conditions and deadlocks</li>
</ul>

<h3>Disadvantages</h3>

<ul>
<li>Depends on purity to work at all</li>
<li>Not really a disadvantage, you're already stuck with purity in Haskell</li>
<li>Not really any other disadvantages, so just use it!</li>
</ul>

</div>

---

## Laziness

*A double edged sword*

Let's revisit our previous summing example

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

There are two problems with this code:

1. There's a major performance bug in it
2. It's much more cumbersome than it should be

NOTE: Kind of cheeky to hold off on laziness this long

----

## Space leaks

Consider `let foo = 1 + 2`

* `foo` isn't `3`, it's an instruction for how to create `3`
* `foo` is a _thunk_ until it's evaluated
* Storing thunks is more expensive than simple types like `Int`s
* Which values are evaluated in our `loop`?

```haskell
let loop i total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

NOTE:

* The bane of laziness is space leaks, which you may have hard
  about. Need to understand how laziness is implemented.
* Explain why `i` is forced and `total` is not
* Builds a tree, lots of CPU and memory pressure

----

## Explicit strictness

Need to tell Haskell compiler to evaluate `total`. Bang!

```haskell
let loop i !total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

* Needing to do this is a downside of Haskell's laziness
* But do we get any benefits in return?

NOTE:

* Can be explicit about what needs to be evaluated
* This is one approach, there are others
* Just added a `!`

----

## Looping (1)

Let's write our `loop` in an imperative language:

```
total := 0
for(i := 1; i <= 1000000; i++) {
  total += i
}
```

Or just the evens

```
total := 0
for(i := 1; i <= 1000000; i++) {
  if (isEven(i)) {
    total += i
  }
}
```

----

## Looping (2)

Now add up the values modulus 13 (for some weird reason)

```
total := 0
for(i := 1; i <= 1000000; i++) {
  if (isEven(i)) {
    total += i % 13
  }
}
```

* Each modification is fine
* Getting harder to see the forest for the trees
* If our logic was more complicated, code reuse would be an issue

NOTE: Example of more complicated use case, writing a lookahead parser

----

## Some better Haskell

Our original recursive implementation sucked

```haskell
let loop i !total =
      if i > 1000000
        then total
        else loop (i + 1) (total + i)
 in loop 1 0
```

<div class="fragment">

<p>But this is great</p>

<pre><code class="haskell">sum [1..1000000]</code></pre>

<ul>
<li>Doesn't it allocate 8mb of ints?</li>
<li>Nope, laziness!</li>
<li>Just a thunk telling us how to get the rest of the list</li>
</ul>

</div>

----

## Composable Haskell

Just the evens?

```haskell
sum (filter even [1..1000000])
```

Modulus 13?

```haskell
sum (map (`mod` 13) (filter even [1..1000000]))
```

* Easy and natural to compose functions in a lazy context
* Avoids doing unnecessary work or using too much memory

NOTE:

* Never using more than a few machine words
* Other GHC optimizations avoid allocating any thunks
* Not covering that today
* Mixed bag, but functional+lazy=declarative, performant

----

## Short circuiting for free

In most languages, `&&` and `||` short circuit

```
foo() && bar()
```

* `bar` only called if `foo` returns `true`

In Haskell: we get that for free from laziness:

```haskell
False && _ = False
True && x = x

True || _ = True
False || x = x
```

See also: `and`, `or`, `all`, `any`

----

### Other downsides

* Laziness means an exception can be hiding in any thunk
* Aka partial functions: `head []`
* Also, some inefficient functions still available, `foldl` vs
  `foldl'`

NOTE:

* Generally partial functions are frowned upon
* But they're still in the language

----

## Summary of laziness

<div style="text-align:left">

<h3>Advantages</h3>

<ul>
<li>More composable code</li>
<li>Get efficient results with high level code</li>
<li>Short-circuiting no longer a special case</li>
</ul>

<h3>Disadvantages</h3>

<ul>
<li>Need to worry about space leaks</li>
<li>Exceptions can be hiding in many places</li>
<li>Bad functions still linger</li>
</ul>

</div>

----

## Side note; other languages

* Laziness is very similar to features in other languages
* Python generators
* Rust iterators
* In Haskell, it's far more prevalent since it affects how all code works
* However, you can get a lot of the benefits of Haskell with these techniques

---

## Other examples

* Too much to talk about in 40 minutes!
* Two other topics I wanted to touch on
* Feel free to ask me about these at breaks

----

## Parser (and other) DSLs

* Operator overloading!
* Abstractions like `Alternative` a natural fit
    * `parseXMLElement <|> parseXMLText`.
* Able to reuse huge number of existing library functions,
    * `optional`, `many`, `foldMap`
* General purpose `do`-notation is great

----

## Parser example

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

----

## Advanced techniques

* Free monads
* Monad transformer stacks
* Lens, conduit, pipes, ...
* Lots of ways to do things in Haskell!
* It's a plus and a minus
* Recommendation: choose a useful subset of Haskell and its libraries,
  and define some best practices

---

## Conclusion

* Haskell combines a lot of uncommon features
* Very few of those features are unique
* Combining those features allows you to write code very differently
  than in other languages
* If you want readable, robust, easy to maintain code: I think it's a
  great choice
* Be aware of the sharp edges: they do exist!

----

## Questions?

Thanks everyone!
