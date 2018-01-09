---
title: Exceptions, transformers, primitive
---

# Exceptions, transformers, primitive

---

## Error handling

Let's discuss the different ways you can handle errors in general

----

## Explicit return checking

Examples: C, Go

```
if err != nil { return err }
```

Explicit, obvious, tedious, easy to forget

----

## Explicit with sum types

Examples: none?

```haskell
eres1 <- doSomething
case eres1 of
  Left e -> handleError1 e
  Right x -> do
    eres2 <- doSomething2 x
    ...
```

* Forced to consider `Left` case...
* Unless you don't care about the result (`putStrLn`)

----

## Sum types with language help

Examples: Rust

```rust
let x = doSomething()?;
let y = doSomething2(x);
```

* Bonus: compiler warning/error on ignored `Result`
* Less tedious, difficult to get wrong
* Have to wrangle different exception types

----

## Monad transformer

Examples: Haskell, others?

```haskell
foo :: ExceptT MyExceptionType IO Y
foo = do
  x <- doSomething
  y <- doSomething2 x
  return y
```

* Explicit in the exception type
* ... assuming no exceptions in `IO` itself
* Still need to wrangle different exception types

----

## Unchecked runtime exceptions

Examples: almost everyone

```haskell
foo :: IO Y
foo = do
  x <- doSomething
  y <- doSomething2 x
  return y
```

* No idea what can go wrong
* No tedium
* Much faster than transformers
* In Haskell, this means `SomeException` is thrown from `IO`, always

----

## Reality in Haskell

* `IO` _does_ have unchecked exceptions
* `ExceptT` over `IO` has `SomeException`
* My argument: `ExceptT` over `IO`, in practice, is a bad idea
* Question: if we designed Haskell from the ground up today, would we
  still include unchecked exceptions?

---

## Nice things in Haskell

This code is nice

```haskell
timeout tenSeconds someHTTPRequest
```

So is this

```haskell
race fileWatcher userCode
```

How do we get this?

----

## Async exceptions

* Send exceptions to a thread from somewhere else
* No way to control what type of exception may be received
* Therefore: any `IO` action can receive an exception of any type
* Necessitates an unchecked exception world
* Once you have that, you have to assume `IO` can fail with anything
  at any time
    * Caveat: you can `mask` temporarily for resource cleanup purposes

----

## Dealing with async exceptions

<https://haskell-lang.org/library/safe-exceptions>

Same approach used by the new unliftio package, docs still in transition

---

## What about transformers?

* Already know `ExceptT` can address async exceptions
* Can we deal with synchronous exceptions in transformer code?
* https://www.snoyman.com/reveal/monad-transformer-state
* https://www.yesodweb.com/blog/2014/05/exceptions-cont-monads

---

## Exception handling best practices

* https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell

---

## The RIO monad

<https://www.fpcomplete.com/blog/2017/07/the-rio-monad>

---

## Still have time for more?

Let's play with some really fun stuff

* https://haskell-lang.org/tutorial/primitive-haskell
* <https://wiki.haskell.org/Evaluation_order_and_state_tokens>
