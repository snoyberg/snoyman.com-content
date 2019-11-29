It's about a year since I wrote the [last installment](https://www.snoyman.com/blog/2018/12/rust-crash-course-07-async-futures-tokio) in the Rust Crash Course series. That last post was a doozy, diving into async, futures, and tokio. All in one post. That was a bit sadistic, and I'm a bit proud of myself on that front.

Much has happened since then, however. Importantly: the `Future` trait has moved into the standard library itself and absorbed a few modifications. And then to tie that up in a nicer bow, there's a new `async/.await` syntax. It's hard for me to overstate just how big a quality of life difference this is when writing asynchronous code in Rust.

I recently [wrote an article on the FP Complete tech site](https://tech.fpcomplete.com/rust/pid1) that demonstrates the `Future` and `async/.await` stuff in practice. But here, I want to give a more thorough analysis of what's going on under the surface. Unlike lesson 7, I'm going to skip the motivation for why we want to write asynchronous code, and break this up into more digestible chunks. Like lesson 7, I'm going to include the exercise solutions inline, instead of a separate post.

__NOTE__ I'm going to use the `async-std` library in this example instead of `tokio`. My only real reason for this is that I started using `async-std` before `tokio` released support for the new `async/.await` syntax. I'm not ready to weigh in on, in general, which of the libraries I prefer.

You should start a Cargo project to play along. Try `cargo new --bin sleepus-interruptus`. If you want to ensure you're on the same compiler version, add a `rust-toolchain` file with the string `1.39.0` in it. Run `cargo run` to make sure you're all good to go.

## Sleepus Interruptus

I want to write a program which will print the message `Sleepus` 10 times, with a delay of 0.5 seconds. And it should print the message `Interruptus` 5 times, with a delay of 1 second. This is some fairly easy Rust code:

```rust
use std::thread::{sleep};
use std::time::Duration;

fn sleepus() {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
}

fn interruptus() {
    for i in 1..=5 {
        println!("Interruptus {}", i);
        sleep(Duration::from_millis(1000));
    }
}

fn main() {
    sleepus();
    interruptus();
}
```

However, as my clever naming implies, this isn't my real goal. This program runs the two operations _synchronously_, first printing `Sleepus`, then `Interruptus`. Instead, we would want to have these two sets of statements printed in an interleaved way. That way, the interruptus actually does some interrupting.

__EXERCISE__ Use the `std::thread::spawn` function to spawn an operating system thread to make these printed statements interleave.

There are two basic approaches to this. One&mdash;maybe the more obvious&mdash;is to spawn a separate thread for each function, and then wait for each of them to complete:

```rust
use std::thread::{sleep, spawn};

fn main() {
    let sleepus = spawn(sleepus);
    let interruptus = spawn(interruptus);

    sleepus.join().unwrap();
    interruptus.join().unwrap();
}
```

Two things to notice:

* We call `spawn` with `spawn(sleepus)`, _not_ `spawn(sleepus())`. The former passes in the function `sleepus` to `spawn` to be run. The latter would immediately run `sleepus()` and pass its result to `spawn`, which is not what we want.
* I use `join()` in the main function/thread to wait for the child thread to end. And I use `unwrap` to deal with any errors that may occur, because I'm being lazy.

Another approach would be to spawn one helper thread instead, and call one of the functions in the main thread:

```rust
fn main() {
    let sleepus = spawn(sleepus);
    interruptus();

    sleepus.join().unwrap();
}
```

This is more efficient (less time spawning threads and less memory used for holding them), and doesn't really have a downside. I'd recommend going this way.

__QUESTION__ What would be the behavior of this program if we didn't call `join` in the two-spawn version? What if we didn't call `join` in the one-spawn version?

But this isn't an asynchronous approach to the problem at all! We have two threads being handled by the operating system which are both acting synchronously and making blocking calls to `sleep`. Let's build up a bit of intuition towards how we could have our two tasks (printing `Sleepus` and printing `Interruptus`) behave more cooperatively in a single thread.

## Introducing `async`

We're going to start at the highest level of abstraction, and work our way down to understand the details. Let's rewrite our application in an `async` style. Add the following to your `Cargo.toml`:

```toml
async-std = { version = "1.2.0", features = ["attributes"] }
```

And now we can rewrite our application as:

```rust
use async_std::task::{sleep, spawn};
use std::time::Duration;

async fn sleepus() {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500)).await;
    }
}

async fn interruptus() {
    for i in 1..=5 {
        println!("Interruptus {}", i);
        sleep(Duration::from_millis(1000)).await;
    }
}

#[async_std::main]
async fn main() {
    let sleepus = spawn(sleepus());
    interruptus().await;

    sleepus.await;
}
```

Let's hit the changes from top to bottom:

* Instead of getting `sleep` and `spawn` from `std::thread`, we're getting them from `async_std::task`. That probably makes sense.
* Both `sleepup` and `interruptus` now say `async` in front of `fn`.
* After the calls to `sleep`, we have a `.await`. Note that this is _not_ a `.await()` method call, but instead a new syntax.
* We have a new attribute `#[async_std::main]` on the `main` function.
* The `main` function also has `async` before `fn`.
* Instead of `spawn(sleepus)`, passing in the function itself, we're now calling `spawn(sleepus())`, immediately running the function and passing its result to `spawn`.
* The call to `interruptus()` is now followed by `.await`.
* Instead of `join()`ing on the `sleepus` `JoinHandle`, we use the `.await` syntax.

__EXERCISE__ Run this code on your own machine and make sure everything compiles and runs as expected. Then try undoing some of the changes listed above and see what generates a compiler error, and what generates incorrect runtime behavior.

That may look like a large list of changes. But in reality, our code is almost identical structural to the previous version, which is a real testament to the `async/.await` syntax. And now everything works under the surface the way we want: a single operating system thread making non-blocking calls.

Let's analyze what each of these changes actually means.

## `async` functions

Adding `async` to the beginning of a function definition does three things:

1. It allows you to use `.await` syntax inside. We'll get to the meaning of that in a bit.
2. It modified the return type of the function. `async fn foo() -> Bar` actually returns `impl std::future::Future<Output=Bar>`.
3. Automatically wraps up the result value in a new `Future`. We'll demonstrate that better later.

Let's unpack that second point a bit. There's a trait called `Future` defined in the standard library. It has an associated type `Output`. What this trait means is: I promise that, when I complete, I will give you a value of type `Output`. You could imagine, for instance, an asynchronous HTTP client that looks something like:

```rust
impl HttpRequest {
    fn perform(self) -> impl Future<Output=HttpResponse> { ... }
}
```

There will be some non-blocking I/O that needs to occur to make that request. We don't want to block the calling thread while those things happen. But we do want to somehow eventually get the resulting response.

We'll play around with `Future` values more directly later. For now, we'll continue sticking with the high-level `async/.await` syntax.

__EXERCISE__ Rewrite the signature of `sleepus` to not use the `async` keyword by modifying its result type. Note that the code will not compile when you get the type right. Pay attention to the error message you get.

The result type of `async fn sleepus()` is the implied unit value `()`. Therefore, the `Output` of our `Future` should be unit. This means we need to write our signature as:

```rust
fn sleepus() -> impl std::future::Future<Output=()>
```

However, with only that change in place, we get the following error messages:

```
error[E0728]: `await` is only allowed inside `async` functions and blocks
 --> src/main.rs:7:9
  |
4 | fn sleepus() -> impl std::future::Future<Output=()> {
  |    ------- this is not `async`
...
7 |         sleep(Duration::from_millis(500)).await;
  |         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ only allowed inside `async` functions and blocks

error[E0277]: the trait bound `(): std::future::Future` is not satisfied
 --> src/main.rs:4:17
  |
4 | fn sleepus() -> impl std::future::Future<Output=()> {
  |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `std::future::Future` is not implemented for `()`
  |
  = note: the return type of a function must have a statically known size
```

The first message is pretty direct: you can only use the `.await` syntax inside an `async` function or block. We haven't seen an `async` block yet, but it's exactly what it sounds like:

```rust
async {
    // async noises intensify
}
```

The second error message is a result of the first: the `async` keyword causes the return type to be an `impl Future`. Without that keyword, our `for` loop evaluates to `()`, which isn't an `impl Future`.

__EXERCISE__ Fix the compiler errors by introducing an `await` block inside the `sleepus` function. Do _not_ add `async` to the function signature, keep using `impl Future`.

Wrapping the entire function body with an `async` block solves the problem:

```rust
fn sleepus() -> impl std::future::Future<Output=()> {
    async {
        for i in 1..=10 {
            println!("Sleepus {}", i);
            sleep(Duration::from_millis(500)).await;
        }
    }
}
```

## `.await` a minute

Maybe we don't need all this `async/.await` garbage though. What if we remove the calls to `.await` usage in `sleepus`? Perhaps surprisingly, it compiles, though it does give us an ominous warning:

```
warning: unused implementer of `std::future::Future` that must be used
 --> src/main.rs:8:13
  |
8 |             sleep(Duration::from_millis(500));
  |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  |
  = note: `#[warn(unused_must_use)]` on by default
  = note: futures do nothing unless you `.await` or poll them
```

We're generating a `Future` value but not using it. And sure enough, if you look at the output of our program, you can see what the compiler means:

```
Interruptus 1
Sleepus 1
Sleepus 2
Sleepus 3
Sleepus 4
Sleepus 5
Sleepus 6
Sleepus 7
Sleepus 8
Sleepus 9
Sleepus 10
Interruptus 2
Interruptus 3
Interruptus 4
Interruptus 5
```

All of our `Sleepus` messages print without delay. Intriguing! The issue is that the call to `sleep` no longer actually puts our current thread to sleep. Instead, it generates a value which implements `Future`. And when that promise is eventually fulfilled, we know that the delay has occurred. But in our case, we're simply ignoring the `Future`, and therefore never actually delaying.

To understand what the `.await` syntax is doing, we're going to implement our function with much more direct usage of the `Future` values. Let's start by getting rid of the `async` block.

## Dropping `async` block

If we drop the `async` block, we end up with this code:

```rust
fn sleepus() -> impl std::future::Future<Output=()> {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
}
```

This gives us an error message we saw before:

```
error[E0277]: the trait bound `(): std::future::Future` is not satisfied
 --> src/main.rs:4:17
  |
4 | fn sleepus() -> impl std::future::Future<Output=()> {
  |                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ the trait `std::future::Future` is not implemented for `()`
  |
```

This makes sense: the `for` loop evaluates to `()`, and unit does not implement `Future`. One way to fix this is to add an expression after the `for` loop that evaluates to something that implements `Future`. And we already know one such thing: `sleep`.

__EXERCISE__ Tweak the `sleepus` function so that it compiles.

One implementation is:

```rust
fn sleepus() -> impl std::future::Future<Output=()> {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
    sleep(Duration::from_millis(0))
}
```

We still get a warning about the unused `Future` value inside the `for` loop, but not the one afterwards: that one is getting returned from the function. But of course, sleeping for 0 milliseconds is just a wordy way to do nothing. It would be nice if there was a "dummy" `Future` that more explicitly did nothing. And fortunately, [there is](https://docs.rs/async-std/1.2.0/async_std/future/fn.ready.html).

__EXERCISE__ Replace the `sleep` call after the `for` loop with a call to `ready`.

```rust
fn sleepus() -> impl std::future::Future<Output=()> {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
    async_std::future::ready(())
}
```

## Implement our own `Future`

To unpeel this onion a bit more, let's make our life harder, and _not_ use the `ready` function. Instead, we're going to define our own `struct` which implements `Future`. I'm going to call it `DoNothing`.

```rust
use std::future::Future;

struct DoNothing;

fn sleepus() -> impl Future<Output=()> {
    for i in 1..=10 {
        println!("Sleepus {}", i);
        sleep(Duration::from_millis(500));
    }
    DoNothing
}
```

__EXERCISE__ This code won't compile. Without looking below or asking the compiler, what do you think it's going to complain about?

The problem here is that `DoNothing` does not provide a `Future` implementation. We're going to do some Compiler Driven Development and let `rustc` tell us how to fix our program. Our first error message is:

```
the trait bound `DoNothing: std::future::Future` is not satisfied
```

So let's add in a trait implementation:

```rust
impl Future for DoNothing {
}
```

Which fails with:

```
error[E0046]: not all trait items implemented, missing: `Output`, `poll`
 --> src/main.rs:7:1
  |
7 | impl Future for DoNothing {
  | ^^^^^^^^^^^^^^^^^^^^^^^^^ missing `Output`, `poll` in implementation
  |
  = note: `Output` from trait: `type Output;`
  = note: `poll` from trait: `fn(std::pin::Pin<&mut Self>, &mut std::task::Context<'_>) -> std::task::Poll<<Self as std::future::Future>::Output>`
```

We don't really know about the `Pin<&mut Self>` or `Context` thing yet, but we do know about `Output`. And since we were previously returning a `()` from our `ready` call, let's do the same thing here.

```rust
use std::pin::Pin;
use std::task::{Context, Poll};

impl Future for DoNothing {
    type Output = ();

    fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
        unimplemented!()
    }
}
```

Woohoo, that compiles! Of course, it fails at runtime due to the `unimplemented!()` call:

```
thread 'async-std/executor' panicked at 'not yet implemented', src/main.rs:13:9
```

We'll get to that executor bit later. For now, let's try to implement `poll`. We need to return a value of type `Poll<Self::Output>`, or `Poll<()>`. Let's look at the [definition of `Poll`](https://doc.rust-lang.org/std/task/enum.Poll.html):

```rust
pub enum Poll<T> {
    Ready(T),
    Pending,
}
```

Using some basic deduction, we can see that `Ready` means "our `Future` is complete, and here's the output" while `Pending` means "it's not done yet." Given that our `DoNothing` wants to return the output of `()` immediately, we can just use the `Ready` variant here.

__EXERCISE__ Implement a working version of `poll`.

```rust
fn poll(self: Pin<&mut Self>, _ctx: &mut Context) -> Poll<Self::Output> {
    Poll::Ready(())
}
```

Congratulations, you've just implemented your first `Future` struct!

## The third `async` difference

Remember above we said that making a function `async` does a third thing:

> Automatically wraps up the result value in a new `Future`. We'll demonstrate that better later.

Now is later. Let's demonstrate that better.

Let's simplify the definition of `sleepus` to:

```rust
fn sleepus() -> impl Future<Output=()> {
    DoNothing
}
```

The compiles and runs just fine. Let's try switching back to the `async` way of writing the signature:

```rust
async fn sleepus() {
    DoNothing
}
```

This now gives us an error:

```
error[E0271]: type mismatch resolving `<impl std::future::Future as std::future::Future>::Output == ()`
  --> src/main.rs:17:20
   |
17 | async fn sleepus() {
   |                    ^ expected struct `DoNothing`, found ()
   |
   = note: expected type `DoNothing`
              found type `()`
```

You see, when you have an `async` function or block, the result is automatically wrapped up in a `Future`. So instead of returning a `DoNothing`, we're returning a `impl Future<Output=DoNothing>`. And our type wants `Output=()`.

__EXERCISE__ Try to guess what you need to add to this function to make it compile.

Working around this is pretty easy: you simply append `.await` to `DoNothing`:

```rust
async fn sleepus() {
    DoNothing.await
}
```

This gives us a little more intuition for what `.await` is doing: it's extracting the `()` `Output` from the `DoNothing` `Future`... somehow. However, we still don't really know how it's achieving that. Let's build up a more complicated `Future` to get closer.

## SleepPrint

We're going to build a new `Future` implementation which:

* Sleeps for a certain amount of time
* Then prints a message

This is going to involve using [pinned pointers](https://doc.rust-lang.org/std/pin/index.html). I'm not going to describe those here. The specifics of what's happening with the pinning isn't terribly enlightening to the topic of `Future`s. If you want to let your eyes glaze over at that part of the code, you won't be missing much.

Our implementation strategy for `SleepPrint` will be to wrap an existing `sleep` `Future` with our own implementation of `Future`. Since we don't know the exact type of the result of a `sleep` call (it's just an `impl Future`), we'll use a parameter:

```rust
struct SleepPrint<Fut> {
    sleep: Fut,
}
```

And we can call this in our `sleepus` function with:

```rust
fn sleepus() -> impl Future<Output=()> {
    SleepPrint {
        sleep: sleep(Duration::from_millis(3000)),
    }
}
```

Of course, we now get a compiler error about a missing `Future` implementation. So let's work on that. Our `impl` starts with:

```rust
impl<Fut: Future<Output=()>> Future for SleepPrint<Fut> {
    ...
}
```

This says that `SleepPrint` is a `Future` if the `sleep` value it contains is a `Future` with an `Output` of type `()`. Which, of course, is true in the case of the `sleep` function, so we're good. We need to define `Output`:

```rust
type Output = ();
```

And then we need a `poll` function:

```rust
fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
    ...
}
```

The next bit is the eyes-glazing part around pinned pointers. We need to _project_ the `Pin<&mut Self>` into a `Pin<&mut Fut>` so that we can work on the underlying sleep `Future`. We could use a [helper crate](https://crates.io/crates/pin-project-lite) to make this a bit prettier, but we'll just do some `unsafe` mapping:

```rust
let sleep: Pin<&mut Fut> = unsafe { self.map_unchecked_mut(|s| &mut s.sleep) };
```

Alright, now the important bit. We've got our underlying `Future`, and we need to do something with it. The only thing e _can_ do with it is call `poll`. `poll` requires a `&mut Context`, which fortunately we've been provided. That `Context` contains information about the currently running task, so it can be woken up (via a `Waker`) when the task is ready. We'll get into that a bit more later.

For now, let's do the only thing we can reasonably do:

```rust
match sleep.poll(ctx) {
    ...
}
```

We've got two possibilities. If `poll` returns a `Pending`, it means that the `sleep` hasn't completed yet. In that case, we want our `Future` to also indicate that it's not done. To make that work, we just propagate the `Pending` value:

```rust
Poll::Pending => Poll::Pending,
```

However, if the `sleep` is already complete, we'll receive a `Ready(())` variant. In that case, it's finally time to print our message and then propagate the `Ready`:

```rust
Poll::Ready(()) => {
    println!("Inside SleepPrint");
    Poll::Ready(())
},
```

And just like that, we've built a more complex `Future` from a simpler one. But that was pretty ad-hoc.

## `and_then`

## TODO

* Implement `Sleepus` struct
* Explain waker, point to `pid1` post
* Explain `main` attr, demonstrate the `block_on` function, explain executors
* Show double-spawning and `await`ing
* Use `join` instead of `.await` in `main`
