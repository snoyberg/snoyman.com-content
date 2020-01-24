It's about a year since I wrote the [last installment](https://www.snoyman.com/blog/2018/12/rust-crash-course-07-async-futures-tokio) in the Rust Crash Course series. That last post was a doozy, diving into async, futures, and tokio. All in one post. That was a bit sadistic, and I'm a bit proud of myself on that front.

Much has happened since then, however. Importantly: the `Future` trait has moved into the standard library itself and absorbed a few modifications. And then to tie that up in a nicer bow, there's a new `async/.await` syntax. It's hard for me to overstate just how big a quality of life difference this is when writing asynchronous code in Rust.

I recently [wrote an article on the FP Complete tech site](https://tech.fpcomplete.com/rust/pid1) that demonstrates the `Future` and `async/.await` stuff in practice. But here, I want to give a more thorough analysis of what's going on under the surface. Unlike lesson 7, I'm going to skip the motivation for why we want to write asynchronous code, and break this up into more digestible chunks. Like lesson 7, I'm going to include the exercise solutions inline, instead of a separate post.

__NOTE__ I'm going to use the `async-std` library in this example instead of `tokio`. My only real reason for this is that I started using `async-std` before `tokio` released support for the new `async/.await` syntax. I'm not ready to weigh in on, in general, which of the libraries I prefer.

You should start a Cargo project to play along. Try `cargo new --bin sleepus-interruptus`. If you want to ensure you're on the same compiler version, add a `rust-toolchain` file with the string `1.39.0` in it. Run `cargo run` to make sure you're all good to go.

This post is part of a series based on [teaching Rust at FP
Complete](https://tech.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

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
* Both `sleepus` and `interruptus` now say `async` in front of `fn`.
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

__EXERCISE__ Fix the compiler errors by introducing an `async` block inside the `sleepus` function. Do _not_ add `async` to the function signature, keep using `impl Future`.

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

Now let's try to implement `poll`. We need to return a value of type `Poll<Self::Output>`, or `Poll<()>`. Let's look at the [definition of `Poll`](https://doc.rust-lang.org/std/task/enum.Poll.html):

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

Alright, now the important bit. We've got our underlying `Future`, and we need to do something with it. The only thing we _can_ do with it is call `poll`. `poll` requires a `&mut Context`, which fortunately we've been provided. That `Context` contains information about the currently running task, so it can be woken up (via a `Waker`) when the task is ready.

__NOTE__ We're not going to get deeper into how `Waker` works in this post. If you want a real life example of how to call `Waker` yourself, I recommend reading my [pid1 in Rust](https://tech.fpcomplete.com/rust/pid1) post.

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

## TwoFutures

`SleepPrint` is pretty ad-hoc: it hard codes a specific action to run after the `sleep` `Future` completes. Let's up our game, and sequence the actions of two different `Future`s. We're going to define a new `struct` that has three fields:

* The first `Future` to run
* The second `Future` to run
* A `bool` to tell us if we've finished running the first `Future`

Since the `Pin` stuff is going to get a bit more complicated, it's time to reach for that helper crate to ease our implementation and avoid `unsafe` blocks ourself. So add the following to your `Cargo.toml`:

```toml
pin-project-lite = "0.1.1"
```

And now we can define a `TwoFutures` struct that allows us to project the first and second `Future`s into pinned pointers:

```rust
use pin_project_lite::pin_project;

pin_project! {
    struct TwoFutures<Fut1, Fut2> {
        first_done: bool,
        #[pin]
        first: Fut1,
        #[pin]
        second: Fut2,
    }
}
```

Using this in `sleepus` is easy enough:

```rust
fn sleepus() -> impl Future<Output=()> {
    TwoFutures {
        first_done: false,
        first: sleep(Duration::from_millis(3000)),
        second: async { println!("Hello TwoFutures"); },
    }
}
```

Now we just need to define our `Future` implementation. Easy, right? We want to make sure both `Fut1` and `Fut2` are `Future`s. And our `Output` will be the output from the `Fut2`. (You could also return both the first and second output if you wanted.) To make all that work:

```rust
impl<Fut1: Future, Fut2: Future> Future for TwoFutures<Fut1, Fut2> {
    type Output = Fut2::Output;

    fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<Self::Output> {
        ...
    }
}
```

In order to work with the pinned pointer, we're going to get a new value, `this`, which projects all of the pointers:

```rust
let this = self.project();
```

With that out of the way, we can interact with our three fields directly in `this`. The first thing we do is check if the first `Future` has already completed. If not, we're going to poll it. If the poll is `Ready`, then we'll ignore the output and indicate that the first `Future` is done:

```rust
if !*this.first_done {
    if let Poll::Ready(_) = this.first.poll(ctx) {
        *this.first_done = true;
    }
}
```

Next, if the first `Future` is done, we want to poll the second. And if the first `Future` is _not_ done, then we say that we're pending:

```rust
if *this.first_done {
    this.second.poll(ctx)
} else {
    Poll::Pending
}
```

And just like that, we've composed two `Future`s together into a bigger, grander, brighter `Future`.

__EXERCISE__ Get rid of the usage of an `async` block in `second`. Let the compiler errors guide you.

The error message you get says that `()` is not a `Future`. Instead, you need to return a `Future` value after the call to `println!`. We can use our handy `async_std::future::ready`:

```rust
second: {
    println!("Hello TwoFutures");
    async_std::future::ready(())
},
```

## AndThen

Sticking together two arbitrary `Future`s like this is nice. But it's even nicer to have the second `Future`s depend on the result of the first `Future`. To do this, we'd want a function like `and_then`. (Monads FTW to my Haskell buddies.) I'm not going to bore you with the gory details of an implementation here, but feel free to [read the Gist if you're interested](https://gist.github.com/snoyberg/7eeb5e330d9b5db9806d82c83c9d3e56). Assuming you have this method available, we can begin to write the `sleepus` function ourselves as:

```rust
fn sleepus() -> impl Future<Output = ()> {
    println!("Sleepus 1");
    sleep(Duration::from_millis(500)).and_then(|()| {
        println!("Sleepus 2");
        sleep(Duration::from_millis(500)).and_then(|()| {
            println!("Sleepus 3");
            sleep(Duration::from_millis(500)).and_then(|()| {
                println!("Sleepus 4");
                async_std::future::ready(())
            })
        })
    })
}
```

And before Rust 1.39 and the `async/.await` syntax, this is basically how async code worked. This is far from perfect. Besides the obvious right-stepping of the code, it's not actually a loop. You _could_ recursively call `sleepus`, except that creates an infinite type which the compiler isn't too fond of.

But fortunately, we've now finally established enough background to easily explain what the `.await` syntax is doing: exactly what `and_then` is doing, but without the fuss!

__EXERCISE__ Rewrite the `sleepus` function above to use `.await` instead of `and_then`.

The rewrite is really easy. The body of the function becomes the non-right-stepping, super flat:

```rust
println!("Sleepus 1");
sleep(Duration::from_millis(500)).await;
println!("Sleepus 2");
sleep(Duration::from_millis(500)).await;
println!("Sleepus 3");
sleep(Duration::from_millis(500)).await;
println!("Sleepus 4");
```

And then we also need to change the signature of our function to use `async`, or wrap everything in an `async` block. Your call.

Besides the obvious readability improvements here, there are some massive usability improvements with `.await` as well. One that sticks out here is how easily it ties in with loops. This was a real pain with the older `futures` stuff. Also, chaining together multiple `await` calls is really easy, e.g.:

```rust
let body = make_http_request().await.get_body().await;
```

And not only that, but it plays in with the `?` operator for error handling perfectly. The above example would more likely be:

```rust
let body = make_http_request().await?.get_body().await?;
```

## `main` attribute

One final mystery remains. What exactly is going on with that weird attribute on `main`:

```rust
#[async_std::main]
async fn main() {
    ...
}
```

Our `sleepus` and `interruptus` functions do not actually do anything. They return `Future`s which provide instructions on how to do work. Something has to actually perform those actions. The thing that runs those actions is an **executor**. The `async-std` library provides an executor, as does `tokio`. In order to run any `Future`, you need an executor.

The attribute above automatically wraps the `main` function with `async-std`'s executor. The attribute approach, however, is totally optional. Instead, you can use `async_std::task::block_on`.

__EXERCISE__ Rewrite `main` to not use the attribute. You'll need to rewrite it from `async fn main` to `fn main`.

Since we use `.await` inside the body of `main`, we get an error when we simply remove the `async` qualifier. Therefore, we need to use an `async` block inside `main` (or define a separate helper `async` function). Putting it all together:

```rust
fn main() {
    async_std::task::block_on(async {
        let sleepus = spawn(sleepus());
        interruptus().await;

        sleepus.await;
    })
}
```

Each executor is capable of managing multiple tasks. Each task is working on producing the output of a single `Future`. And just like with threads, you can `spawn` additional tasks to get concurrent running. Which is exactly how we achieve the interleaving we wanted!

## Cooperative concurrency

One word of warning. `Future`s and `async`/`.await` implement a form of cooperative concurrency. By contrast, operating system threads provide preemptive concurrency. The important different is that in cooperative concurrency, you have to cooperate. If one of your tasks causes a delay, such as by using `std::thread::sleep` or by performing significant CPU computation, it will not be interrupted.

The upshot of this is that you should ensure you do not perform blocking calls inside your tasks. And if you have a CPU-intensive task to perform, it's probably worth spawning an OS thread for it, or at least ensuring your executor will not starve your other tasks.

## Summary

I don't think the behavior under the surface of `.await` is too big a reveal, but I think it's useful to understand exactly what's happening here. In particular, understanding the difference between a value of `Future` and actually chaining together the outputs of `Future` values is core to using `async/.await` correctly. Fortunately, the compiler errors and warnings do a great job of guiding you in the right direction.

In the next lesson, we can start using our newfound knowledge of `Future` and the `async/.await` syntax to build some asynchronous applications. We'll be diving into writing some async I/O, including networking code, using Tokio 0.2.

## Exercises

Here are some take-home exercises to play with. You can base them on [the code in this Gist](https://gist.github.com/snoyberg/f5fea804f2b6fb69ae6d1f75c8004fc5).

1. Modify the `main` function to call `spawn` twice instead of just once.
2. Modify the `main` function to not call `spawn` at all. Instead, use [`join`](https://docs.rs/async-std/1.2.0/async_std/future/trait.Future.html#method.join). You'll need to add a `use async_std::prelude::*;` and add the `"unstable"` feature to the `async-std` dependency in `Cargo.toml`.
3. Modify the `main` function to get the non-interleaved behavior, where the program prints `Sleepus` multiple times before `Interruptus`.
4. We're still performing blocking I/O with `println!`. Turn on the `"unstable"` feature again, and try using `async_std::println`. You'll get an ugly error message until you get rid of `spawn`. Try to understand why that happens.
5. Write a function `foo` such that the following assertion passes: `assert_eq!(42, async_std::task::block_on(async { foo().await.await }));`
