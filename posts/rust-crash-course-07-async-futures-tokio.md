__NOTE__ With the addition of async/await syntax in Rust 1.39 (November 2019), everything related to async code in Rust is getting an overhaul. As such, this lesson is now pretty deeply out of date. It's still useful for understanding the deeper principles, but I hope to write up an updated tutorial in the future covering the new approach.

Unlike languages like Haskell, Erlang, and Go, Rust does not have a
runtime system providing green threads and asynchronous I/O. However,
for many real world use cases, async I/O is strongly desired, if not a
hard requirement. The de facto standard library for handling this in
Rust is tokio.

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

This lesson in the crash course is going to be a bit different from
others, since:

1. There's a lot of interconnected material to cover which can't as
   easily be separated out
2. It's more important to understand the motivation behind the design
   of these libraries than in many other cases
3. I believe the material may be useful for people who haven't been
   following the rest of the crash course

Therefore, some different rules will apply:

* I'm defining up front the knowledge you'll need of Rust to
  understand this lesson, namely:
    * All the basics of syntax
    * Traits and associated types
    * Iterators
    * Closures
* Instead of providing the exercise solutions in a later post, I'll be
  providing them immediately, since the material is so cumulative. I
  still _strongly recommend_ spending significant time and effort
  trying to solve the exercises yourself before looking at the
  solutions. It's harder and more time consuming, but ultimately
  worthwhile.

Also, this lesson is _much longer and more involved_ than previous
lessons. You should plan on it taking more time to complete than
others. I considered breaking this up into multiple lessons, but
decided to keep all of the content together. Instead, I'll be taking a
break from weekly lessons after this one for a bit.

Consider that the intro to the intro. Now the real intro!

## Why async?

I'm going to assume that readers are already familiar with async I/O
and its motivations in general. If you're not, it's worth reading up a
bit on [the C10k problem](https://en.wikipedia.org/wiki/C10k_problem),
where many of us started thinking hard about async I/O. You may also
be interested in reading a post I wrote [about green
threads](https://www.fpcomplete.com/blog/2017/01/green-threads-are-like-garbage-collection),
a language runtime-based solution to the same problem.

At the end of the day, the goals of Rust's approach to async I/O are:

* Minimize system resources for handling a large number of concurrent
  I/O tasks
* Provide a zero-cost abstraction on top of the async I/O mechanisms
  provided by operating systems
* Do it at a library level, instead of introducing a runtime to Rust

## A sample problem

It's difficult to dive in to the wonderful world of tokio. You need to
learn about futures and streams, tasks and executors, async I/O system
calls and the `Async` type, etc. To try and decouple this learning
experience, we're going to start with a simplified problem. This
problem mostly, but not perfectly, models async I/O in the real world,
and will demonstrate many of the design concerns. It will also let us
play just a bit more with concurrent programming before diving into
futures and tokio.

We're going to run a separate thread. This thread will have access to
two atomic values:

* An `AtomicBool` to tell us whether we want this side thread to keep
  running
* An `AtomicUsize` counter

We haven't covered atomic types yet, but they're exactly what they
sound like: variables that can be safely accessed from multiple
threads. Since learning about them isn't the point of this lesson,
I'll defer further questions about the usage of these types to [their
API
documentation](https://doc.rust-lang.org/std/sync/atomic/index.html).

Our separate thread is going to run in a loop. As long as the
`AtomicBool` is `true`, it will:

* Sleep for a given number of milliseconds
* Print a message to the console
* Increment the `AtomicUsize` counter

The series of examples we'll be looking at will then try different
approaches to observing the changes in the counter.

## Interval

We're going to call this thing an `Interval`, since it somewhat
represents a `setInterval` call in Javascript.  Go ahead and start a
new project:

```
$ cargo new interval --bin
$ cd interval
```

We're going to put the code for our `Interval` into a separate
module. First, put the following code into `src/interval.rs`:

```rust
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread::{sleep, spawn};
use std::time::Duration;

#[derive(Clone)]
pub struct Interval {
    counter: Arc<AtomicUsize>,
    still_running: Arc<AtomicBool>,
}

impl Drop for Interval {
    fn drop(&mut self) {
        println!("Interval thread shutting down");
        self.still_running.store(false, Ordering::SeqCst);
    }
}

impl Interval {
    pub fn from_millis(millis: u64) -> Interval {
        let duration = Duration::from_millis(millis);

        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = counter.clone();

        let still_running = Arc::new(AtomicBool::new(true));
        let still_running_clone = still_running.clone();

        spawn(move || {
            println!("Interval thread launched");
            while still_running_clone.load(Ordering::SeqCst) {
                sleep(duration);
                let old = counter_clone.fetch_add(1, Ordering::SeqCst);
                println!("Interval thread still alive, value was: {}", old);
            }
        });

        Interval {
            counter,
            still_running,
        }
    }

    pub fn get_counter(&self) -> usize {
        self.counter.load(Ordering::SeqCst)
    }
}
```

Next, let's provide a minimal `src/main.rs` which uses this `Interval`
data type:

```rust
mod interval;

use self::interval::Interval;

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let duration = std::time::Duration::from_millis(2000); // 2 seconds
    for i in 1..11 {
        println!("Iteration number {}, counter is {}", i, interval.get_counter());
        std::thread::sleep(duration);
    }
}
```

You should see something like the following:

```
Iteration number 1, counter is 0
Interval thread launched
Interval thread still alive, value was: 0
Interval thread still alive, value was: 1
Interval thread still alive, value was: 2
Iteration number 2, counter is 3
Interval thread still alive, value was: 3
Interval thread still alive, value was: 4
...
Interval thread still alive, value was: 33
Interval thread still alive, value was: 34
Iteration number 10, counter is 35
Interval thread still alive, value was: 35
Interval thread still alive, value was: 36
Interval thread still alive, value was: 37
Interval thread still alive, value was: 38
Interval thread shutting down
```

Hurrah, we have some concurrent communication.

## Problems with this approach

The first thing that jumps out as a problem is that we're missing some updates in the main thread. Notice how the counter jumps from 0 to 3. This is obviously a problem with the interval set in the main thread: we're delaying for 2 seconds instead of half a second. Let's instead delay for a tenth of a second (100ms) in the main thread, and check if the value has changed since last time.

__NOTE__ It's still possible we'll miss some updates this way, since
`sleep` guarantees a thread will sleep _at least_ a given amount of
time, but may sleep longer. However, by having such a large
difference, we're fairly certain we'll catch all of the updates.

```rust
fn main() {
    let interval = Interval::from_millis(500); // half a second
    let duration = std::time::Duration::from_millis(100); // 0.1 seconds
    let mut last = interval.get_counter();
    for i in 1..51 {
        let curr = interval.get_counter();

        if curr != last {
            last = curr;
            println!("Iteration number {}, counter is {}", i, curr);
        }

        std::thread::sleep(duration);
    }
}
```

I had to increase the number of iterations to 50, because so many of
our main thread iterations end up showing no change in the
counter. Here's an example of running this on my machine:

```
Interval thread launched
Interval thread still alive, value was: 0
Iteration number 6, counter is 1
Interval thread still alive, value was: 1
Iteration number 11, counter is 2
Interval thread still alive, value was: 2
Iteration number 16, counter is 3
Interval thread still alive, value was: 3
Iteration number 21, counter is 4
Interval thread still alive, value was: 4
Iteration number 26, counter is 5
Interval thread still alive, value was: 5
Iteration number 31, counter is 6
Interval thread still alive, value was: 6
Iteration number 36, counter is 7
Interval thread still alive, value was: 7
Iteration number 41, counter is 8
Interval thread still alive, value was: 8
Iteration number 46, counter is 9
Interval thread still alive, value was: 9
Interval thread shutting down
```

We didn't lose any counter updates here, but from the bumps in
interval thread numbers, we can see that we're wasting a lot of time
in the main thread checking numbers that aren't changing.

Another problem that's less obvious is that we're dedicating an entire
OS thread to this sleep-and-check iteration. In our simple program,
that's not a big deal. But imagine we decided we wanted to have 50
different similar tasks going on. It would require 49 extra threads,
most of which would sit around `sleep`ing the majority of the
time. That's highly wasteful. We should be able to do better.

Finally, and perhaps least important for the moment, this is all
rather ad hoc. It seems like a common need to be able to abstract over
"this thing will produce a value in the future." Even though this
seems like the least important problem, we'll start by solving it
first.

## The Future trait

Who'd have thought that our meandering would naturally lead to one of
the topics mentioned in this lesson's title! You may have noticed a
pattern that developed in the `main` thread's loop:

* Check if a new value is available
* Use it if it is available
* Skip if it isn't available

That's exactly what the `Future` trait allows us to do, with one
addition: it also allows for error handling. We're not going to worry
about that for now, since my code doesn't have any errors :).

We'll start by adding the `futures` crate as a dependency. In
`Cargo.toml`:

```toml
[dependencies]
futures = "0.1"
```

Next, let's add a new module to provide a struct that will provide a
`Future` implementation. Behold, `src/future.rs`:

```rust
extern crate futures;

use super::interval::Interval;
use futures::prelude::*;

pub struct IntervalFuture {
    interval: Interval,
    last: usize,
}

impl IntervalFuture {
    pub fn new(interval: Interval) -> IntervalFuture {
        let last = interval.get_counter();
        IntervalFuture { interval, last }
    }
}

impl Future for IntervalFuture {
    type Item = usize;
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let curr = self.interval.get_counter();
        if curr == self.last {
            Ok(Async::NotReady)
        } else {
            self.last = curr;
            Ok(Async::Ready(curr))
        }
    }
}
```

We're going to own an `Interval` and the last value we provided, just
like our `main` loop used to. The `new` method is fairly
straightforward. For the `impl Future`, we need to define three
things:

* The thing which will be returned by this type when ready. In our
  case, it's the counter value, which is a `usize`.
* The type of errors that can occur. We don't have any errors, so we
  use `()`. (The Haskeller in me screams that we should use
  `Void`. Soon enough, we'll be able to use
  [`never`](https://doc.rust-lang.org/std/primitive.never.html).)
* A function `poll`, which returns a `Result`. In the error case, this
  will be our `Self::Error`. In the success case, this is an `Async`
  enum type. As we can see in our method body, this is either a
  `Ready` variant with the value, or `NotReady`.

The logic in our function is the same as before, so I won't comment on
implementation. Back in our `src/main.rs`, instead of playing around
with the `curr`/`last` logic, we can just pattern match on the result
of `poll()`ing:

```rust
extern crate futures;

mod future;
mod interval;

use self::interval::Interval;
use self::future::IntervalFuture;
use futures::prelude::*;

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let mut interval_future = IntervalFuture::new(interval);
    let duration = std::time::Duration::from_millis(100); // 0.1 seconds

    for i in 1..51 {
        match interval_future.poll() {
            Ok(Async::Ready(curr)) => {
                println!("Iteration number {}, counter is {}", i, curr);
            }
            Ok(Async::NotReady) => (),
            Err(()) => unreachable!(),
        }

        std::thread::sleep(duration);
    }
}
```

Arguably a minor improvement on the previous code, though nothing
major. But congratulations, you're now officially using the `futures`
crate!

### The Poll type definition

Just a minor helper to mention. The type `Result<Async<Self::Item>,
Self::Error>` may look a bit unwieldy to you. If so, you'll be happy
to learn about the `Poll` type definition, which lets you replace the
above with `Poll<Self::Item, Self::Error>`. Not a big deal, but
important to recognize as you're reading other code.

## The tokio executor

Right now, we're running our own executor in our `main` function:
we're manually looping, delaying, etc. Besides tedium, we've already
mentioned some downsides to this above:

* We need a single thread per task we wish to perform
* We need to implement some kind of guess-and-check thread sleeping

It's time to pull out the big guns, and tokio this thing. We'll end up
losing some functionality first, and then we'll build it back.

First, add `tokio = "0.1"` to your `Cargo.toml`. Now, let's try using
the tokio executor by calling `tokio::run` on a `Future`:

```rust
extern crate futures;
extern crate tokio;

mod future;
mod interval;

use self::interval::Interval;
use self::future::IntervalFuture;
use tokio::prelude::*;

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let mut interval_future = IntervalFuture::new(interval);

    tokio::run(interval_future)
}
```

This fails with a compilation error:

```rust
error[E0271]: type mismatch resolving `<future::IntervalFuture as futures::Future>::Item == ()`
  --> src/main.rs:15:5
   |
15 |     tokio::run(interval_future)
   |     ^^^^^^^^^^ expected usize, found ()
   |
   = note: expected type `usize`
              found type `()`
   = note: required by `tokio::run`
```

The `tokio::run` function expects a `Future` where the `Item` is `()`,
but ours is `usize`. This kind of makes sense anyway: don't we want to
write some code to actually do something when we get a value?

We're going to fix this, first the overly painful way, and then the
pleasant way. That will also help you appreciate why lesson 5 spent so
much time on closures.

### Define an adapter Future

Remember how you can define `Iterator`s that consume other `Iterator`s
and compose more powerful streams? Well, you can do the same thing
with `Future`s. Let's define a new type that will:

* Wrap around an `IntervalFuture`
* Print the new value whenever it's ready

We'll put this in `src/main.rs` for now, it won't last long anyway.

```rust
extern crate futures;
extern crate tokio;

mod future;
mod interval;

use self::interval::Interval;
use self::future::IntervalFuture;
use tokio::prelude::*;

struct IntervalPrinter(IntervalFuture);

impl Future for IntervalPrinter {
    type Item = ();
    type Error = ();
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self.0.poll() {
            Ok(Async::Ready(curr)) => {
                println!("Counter is: {}", curr);
                Ok(Async::Ready(()))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(e) => Err(e),
        }
    }
}

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let interval_future = IntervalFuture::new(interval);
    let interval_printer = IntervalPrinter(interval_future);

    tokio::run(interval_printer)
}
```

Compile the code, but don't run it yet. This is relatively
straight-forward given all of the types and traits we've seen, but
it's obviously tedious. Let's start off with a minor simplification.

### The try_ready macro

The body of poll spends a lot of lines of code to:

* Pattern match
* If it's `NotReady`, return `NotReady`
* If it's an `Err`, return the `Err`

This is a repetitive pattern, and is pretty similar to the error
handling we saw previously in lesson 3. The futures crate provides a
macro, `try_ready!`, to deal with this annoyance. Add the following
above the `extern crate futures;` in `src/main.rs`:

```rust
#[macro_use]
```

And then your implementation of `poll` can be simplified to:

```rust
let curr = try_ready!(self.0.poll());
println!("Counter is: {}", curr);
Ok(Async::Ready(()))
```

Nice! Compile, and again, don't run. (Getting curious yet why I keep
saying that? We'll find out soon, just one more pitstop first.)

### I need some closure

It's amazing I've made it to lesson 7 in this crash course without
making that pun. Obviously, defining an entire struct and `Future`
implementation is a bit overkill to just print a line. Fortunately,
the authors of the `futures` crate noticed this too. There are a
number of combinators built into the `Future` trait that make it easy
to chain things together. We're going to use the `and_then` method:

```rust
extern crate futures;
extern crate tokio;

mod future;
mod interval;

use self::interval::Interval;
use self::future::IntervalFuture;
use tokio::prelude::*;

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let interval_future = IntervalFuture::new(interval);
    let interval_printer = interval_future.and_then(|curr| {
        println!("Counter is: {}", curr);
        futures::future::ok(())
    });

    tokio::run(interval_printer)
}
```

That's much nicer! If you're anything like me though, the
`futures::future::ok(())` is bothering you. What purpose does it serve
there? This is a vital part of the design of futures, which we'll be
taking advantage of quite a bit going forward. It allows us to create
chains of actions to run as each bit of async I/O completes. For now,
we don't want to do anything else after we print the first value from
the counter, so we just return `futures::future::ok(())`, which means
"don't do anything, and return the item `()`".

__Exercise 1__ There's another method, `.map`, which is actually a
better choice for us here than `.and_then`. Try rewriting the code
above to use `.map`. Note: no solution provided to this one.

Out of curiosity, what's the type of `interval_printer`? Let's use the
dumb trick from before of giving it the wrong type. Insert something
silly like `let interval_printer: bool = ...` and try compiling,
you'll get some type like:

```
futures::AndThen<
  future::IntervalFuture,
  futures::FutureResult<(), ()>,
  [closure@src/main.rs:14:59: 17:6]
>
```

If this is starting to look a bit like `Iterator` types, that's by
design. Just like `Iterator`s, `Future`s capture a large amount of
information in the types themselves about what they'll do. This allows
`Future`s to compile down to highly efficient code, living up to the
Rust mantra of zero-cost abstractions.

### Finally, run it!

Is the suspense killing you? Alright, your moment has arrived. What
happens when you run `cargo run`?

```
$ cargo run
Interval thread launched
Interval thread shutting down
Interval thread still alive, value was: 0
...
```

And that's it. It hangs. It doesn't print out the message we
painstakingly added with `and_then`. I'm a complete failure, my life
is in ruins.

After an hour to myself to contemplate my life and a few shots of
whiskey, things started to get clear. In our original implementation,
we had a really wasteful loop in the main function. It kept checking
if the counter had changed. We did that with our `Future`
implementation too at first, sleeping and then checking again for a
`NotReady`. But tokio probably _isn't_ doing that, right? (The answer
is yes, right.)

Instead of doing the silly wasteful thing, the `futures` crate is far
smarter. It has a mechanism to:

* determine which task is trying to get access to the data provided by
  this future, and then
* notify that task that new data is available

The
[`futures::task::current()`](https://docs.rs/futures/0.1/futures/task/fn.current.html)
function gives us the current task being run, as a
[`Task`](https://docs.rs/futures/0.1/futures/task/struct.Task.html)
struct. That struct has a method,
[`notify`](https://docs.rs/futures/0.1/futures/task/struct.Task.html#method.notify),
to let the task know that more data is available.

In our program, we have the logic split between `Interval` and
`IntervalFuture`. `IntervalFuture` will need to be responsible for
calling the `current()` function (give some thought as to why that's
the case). The changes needed to make this work are:

* Add a new field to `Interval` to hold an `Arc<Mutex<Option<Task>>>`
  (yes, that's a mouthful), and initialize correctly.
* Each time we call `fetch_add` and update the counter, also call
  `notify()` on that task, if it's there.
* Provide a method `set_task` to set the `Task` on an `Interval`
* When returning `NotReady` in `IntervalFuture`, call `set_task`

__Exercise 2__ Take a stab at implementing these four changes before
looking at the solution below. A hint on some features of Rust we
haven't covered yet: you'll end up wanting to pattern match on the
`Option` held inside the `Mutex`. You'll want to pattern match by
reference, which will require some code that looks like `Some(ref
task) =>`. And the final output should look like:

```
Interval thread launched
Interval thread still alive, value was: 0
Counter is: 1
Interval thread shutting down
```

If you want to be sure, you can see the [initial version of the code on Github](https://github.com/snoyberg/rush-crash-course-tokio-exercise-2/tree/03c5b9029263ce36e626e877e986a281c9334d4e).

### Solution 2

You can [check out the
diff](https://github.com/snoyberg/rush-crash-course-tokio-exercise-2/commit/1c58bf4df946763f5f9f0773235e975968260ba9)
and [full
solution](https://github.com/snoyberg/rush-crash-course-tokio-exercise-2/tree/1c58bf4df946763f5f9f0773235e975968260ba9)
on Github. Here's the diff included inline:

```diff
diff --git a/src/future.rs b/src/future.rs
index 9aaee3c..e231e7b 100644
--- a/src/future.rs
+++ b/src/future.rs
@@ -22,6 +22,8 @@ impl Future for IntervalFuture {
     fn poll(&mut self) -> Result<Async<Self::Item>, Self::Error> {
         let curr = self.interval.get_counter();
         if curr == self.last {
+            let task = futures::task::current();
+            self.interval.set_task(task);
             Ok(Async::NotReady)
         } else {
             self.last = curr;
diff --git a/src/interval.rs b/src/interval.rs
index 044e2ca..8013ac6 100644
--- a/src/interval.rs
+++ b/src/interval.rs
@@ -1,12 +1,14 @@
 use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
-use std::sync::Arc;
+use std::sync::{Arc, Mutex};
 use std::thread::{sleep, spawn};
 use std::time::Duration;
+use futures::task::Task;

 #[derive(Clone)]
 pub struct Interval {
     counter: Arc<AtomicUsize>,
     still_running: Arc<AtomicBool>,
+    task: Arc<Mutex<Option<Task>>>,
 }

 impl Drop for Interval {
@@ -26,22 +28,37 @@ impl Interval {
         let still_running = Arc::new(AtomicBool::new(true));
         let still_running_clone = still_running.clone();

+        let task: Arc<Mutex<Option<Task>>> = Arc::new(Mutex::new(None));
+        let task_clone = task.clone();
+
         spawn(move || {
             println!("Interval thread launched");
             while still_running_clone.load(Ordering::SeqCst) {
                 sleep(duration);
                 let old = counter_clone.fetch_add(1, Ordering::SeqCst);
                 println!("Interval thread still alive, value was: {}", old);
+
+                let task = task_clone.lock().unwrap();
+                match *task {
+                    None => (),
+                    Some(ref task) => task.notify(),
+                };
             }
         });

         Interval {
             counter,
             still_running,
+            task,
         }
     }

     pub fn get_counter(&self) -> usize {
         self.counter.load(Ordering::SeqCst)
     }
+
+    pub fn set_task(&mut self, task: Task) {
+        let mut guard = self.task.lock().unwrap();
+        *guard = Some(task);
+    }
 }
```

Hurrah, we finally have a working tokio program!

In case you're worried about how complex that was, don't
be. Notification is a vital aspect of how tokio works
internally. However, in most cases, you won't be creating your own
primitive `Future`s, but instead dealing with existing ones provided
by tokio or other libraries. Those existing `Future`s will provide the
necessary notification logic. You'll simply need to obey this one
rule:

> Only return a `NotReady` from a `poll` function if you received a
> `NotReady` from an underlying `Future`.

## Just one value?

It's a bit disappointing that our wonderful long running counter only
ends up printing a single value. Can we create some kind of a loop? A
simple approach like the following doesn't work:

```rust
let interval_printer = interval_future.and_then(|curr| {
    println!("Counter is: {}", curr);
    interval_printer
});
```

This isn't Haskell, we can't recursively refer to `interval_printer`
we're in the middle of defining. Go ahead and take a few other stabs
at doing something like that, and you'll eventually get frustrated and
go back to the whiskey. Digging through the `futures` docs, a helper
function like
[`loop_fn`](https://docs.rs/futures/0.1.25/futures/future/fn.loop_fn.html)
looks promising, but I didn't see a simple way to make it work in this
case. (Please let me know if I missed something!) I ended up with
something wonky like this before stopping:

```rust
fn main() {
    let interval = Interval::from_millis(500); // half a second
    let interval_future = Arc::new(Mutex::new(IntervalFuture::new(interval)));
    let interval_printer = loop_fn(interval_future, |interval_future| {
        let interval_future_clone = interval_future.clone();
        interval_future.lock().unwrap().and_then(|curr| {
            println!("Counter: {}", curr);
            futures::future::ok(Continue(interval_future_clone))
        })
    });

    tokio::run(interval_printer)
}
```

### Another struct!

Like before, we're going to define another helper type to implement
this concept of looping. Then we'll see that this problem has already
been solved better in the `futures` crate itself, but we'll get there
soon.

We want to define a new struct, `KeepPrinting`, which is a newtype
around an `IntervalFuture`. It's going to:

* Have a `Future` implementation
* Have `Item = ()`
* Use a `loop` in its implementation
* Use the `try_ready!` macro

__Exercise 3__ Try implementing `KeepPrinting` and using it in the
`main` function. Solution follows immediately, but try not to cheat!

```rust
#[macro_use]
extern crate futures;
extern crate tokio;

mod future;
mod interval;

use self::future::IntervalFuture;
use self::interval::Interval;
use tokio::prelude::*;

struct KeepPrinting(IntervalFuture);

impl Future for KeepPrinting {
    type Item = ();
    type Error = ();
    fn poll(&mut self) -> Poll<(), ()> {
        loop {
            let curr = try_ready!(self.0.poll());
            println!("Counter: {}", curr);
        }
    }
}

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let interval_future = IntervalFuture::new(interval);
    let keep_printing = KeepPrinting(interval_future);

    tokio::run(keep_printing)
}
```

And, just like that, we get an infinitely looping program. This almost
looks like something that we could have done with `Iterator`s. Which
makes me wonder... is there something like `Iterator`s in `futures`?

## Streams

A `Future` is an action with a delayed single result. A `Stream` is a
stream of results, like an `Iterator`, with a delay between each
value.

In `src/main.rs`, add `mod stream;` and then edit `src/stream.rs`. The
file will end up looking remarkably similar to `src/future.rs`,
except:

* Call the struct `IntervalStream` instead of `IntervalFuture`
* Provide an `impl Stream for IntervalStream` instead of `impl Future`
* Follow the compiler errors to fix it

Within the `main` function, instead of using `KeepPrinting` or
anything else, we'll want to create an `IntervalStream`
value. However, `tokio::run` needs a `Future`, not a `Stream`, to
run. Fortunately, there's a helper function, `for_each`, that runs a
given `closure` on each value in the stream.

__Exercise 4__ Try to implement `src/stream.rs` and
`src/main.rs`. Solution to follow.

The trickiest bit for me when first learning `for_each` was to realize
that, like `and_then`, it needs to end with a `Future`. I don't know
if that was just my own shortcoming, or a common issue. In any event,
if you struggled to realize you needed something like `future::ok(())`
at the end of your closure, you're in good company.

In addition, the `poll` function for a `Stream` is slightly different,
in returning an `Option<Item>`. This is similar to how `Iterator`
works. In our case, we have an infinite stream, so we never provide
the `None` case.

Anyway, here's `src/stream.rs`:

```rust
extern crate futures;

use super::interval::Interval;
use futures::prelude::*;

pub struct IntervalStream {
    interval: Interval,
    last: usize,
}

impl IntervalStream {
    pub fn new(interval: Interval) -> IntervalStream {
        let last = interval.get_counter();
        IntervalStream { interval, last }
    }
}

impl Stream for IntervalStream {
    type Item = usize;
    type Error = ();

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        let curr = self.interval.get_counter();
        if curr == self.last {
            let task = futures::task::current();
            self.interval.set_task(task);
            Ok(Async::NotReady)
        } else {
            self.last = curr;
            Ok(Async::Ready(Some(curr)))
        }
    }
}
```

And `src/main.rs`:

```rust
extern crate futures;
extern crate tokio;

mod interval;
mod stream;

use self::interval::Interval;
use self::stream::IntervalStream;
use tokio::prelude::*;

fn main() {
    let interval = Interval::from_millis(500); // half a second
    let interval_stream = IntervalStream::new(interval);
    let future = interval_stream.for_each(|curr| {
        println!("Counter: {}", curr);
        futures::future::ok(())
    });

    tokio::run(future)
}
```

__Exercise 5__ Like `Iterator`s, `Stream`s have helper methods that
you can use to build up more complex things. For example, try throwing
in `map` and `take` to print only the first 10 counter values, but
double them before printing. (No solution provided.)

This is all beginning to fit together nicely! While there are still
details to learn in the `futures` crate, you've got most of the big
ideas down. The next bit is to get familiar with the API in tokio, but
relatively speaking this is less mind-bending. To hammer home what
we've done so far, we'll hit a few exercises, and then continue with
tokio.

## Exercise 6

Define a new struct `MyOk` such that this `main` function works:

```rust
fn main() {
    let name = String::from("Alice");
    let future = MyOk::new(name).and_then(|name| {
        println!("Name: {}", name);
        MyOk::new(())
    });

    tokio::run(future)
}
```

Hint: before cheating and looking at the solution, here's one piece of
help: you'll want an `Option` inside the `MyOk` newtype, and it's
invalid to call `poll` on it twice.

### Solution 6

```rust
struct MyOk<T>(Option<T>);

impl<T> MyOk<T> {
    fn new(t: T) -> MyOk<T> {
        MyOk(Some(t))
    }
}

impl<T> Future for MyOk<T> {
    type Item = T;
    type Error = ();
    fn poll(&mut self) -> Poll<T, ()> {
        Ok(Async::Ready(self.0.take().unwrap()))
    }
}
```

## Exercise 7

Use `iter_ok` to convert the range `1..11` to a `Stream`, and then
collect it as a `Vec` and print it.

### Solution 7

```rust
fn main() {
    tokio::run(stream::iter_ok(1..11).collect().and_then(|x| {
        println!("{:?}", x);
        future::ok(())
    }))
}
```

## Async I/O

We've played around with the `futures` crate by creating a fake async
I/O source of data (the `Interval`). We've built up `Future`s and
`Stream`s in that world. And we've used tokio's executor to run these
things. It's now time to take it to use some real async I/O.

Most async I/O we care about will end up being network
traffic. Filesystem operations don't always play nicely with async I/O
at an operating system level. That said, to get our feet wet, let's
play with a filesystem based example.

You'll want to look at the docs quite a bit, you can [find them on
docs.rs](https://docs.rs/tokio/0.1.11/tokio/).

## List files in a directory

If you look through the docs above, you may find the function
[`read_dir`](https://docs.rs/tokio/0.1.11/tokio/fs/fn.read_dir.html). It
takes a path, and returns a `ReadDirFuture`. This is a standard
approach in tokio, like we had with `Iterator`s: simple wrapper
functions providing access to the structs that do the heavy
lifting. One thing to get used to in tokio is how to read these docs.

Click through on the `ReadDirFuture` struct. It has a `Future`
implementation, where `Item` is `ReadDir`, and `Error` is
`std::io::Error`. Before we deal with that `ReadDir`, let's just get
something that compiles. Since this is still a crash course, we'll
bash our heads against a brick wall each step of the way.

First, to call `read_dir`, we need a directory. Let's use `"."` (the
current directory). We'll use command line arguments later. Here's a
naive implementation:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;

fn main() {
    let future = fs::read_dir(".");
    tokio::run(future)
}
```

This gives us a somewhat intimidating error message:

```
error[E0271]: type mismatch resolving `<tokio_fs::read_dir::ReadDirFuture<&str> as tokio::prelude::Future>::Item == ()`
 --> src/main.rs:8:5
  |
8 |     tokio::run(future)
  |     ^^^^^^^^^^ expected struct `tokio_fs::read_dir::ReadDir`, found ()
  |
  = note: expected type `tokio_fs::read_dir::ReadDir`
             found type `()`
  = note: required by `tokio::run`

error[E0271]: type mismatch resolving `<tokio_fs::read_dir::ReadDirFuture<&str> as tokio::prelude::Future>::Error == ()`
 --> src/main.rs:8:5
  |
8 |     tokio::run(future)
  |     ^^^^^^^^^^ expected struct `std::io::Error`, found ()
  |
  = note: expected type `std::io::Error`
             found type `()`
  = note: required by `tokio::run`
```

However, let me narrow that down for you:

```
expected type `tokio_fs::read_dir::ReadDir`, found type `()`
expected type `std::io::Error`, found type `()`
```

Oh, right! `tokio::run` requires that we have `Item` and `Error` as
`()`. We can modify the `Error` with `map_err`. Let's just print out
the error if one occurs:

```rust
let future = fs::read_dir(".")
    .map_err(|e| eprintln!("Error reading directory: {}", e))
    ;
```

That knocked out the first compilation error. Let's also throw in a
`.and_then`:

```rust
.and_then(|readdir| {
    println!("FIXME: use this: {:?}", readdir);
})
```

Uh oh, we got this compilation error. Can you figure out how to solve it?

```
error[E0277]: the trait bound `(): tokio::prelude::Future` is not satisfied
```

In my experience, when you see that, it almost always means: "I forgot
to add `future::ok(())`. Remember, `and_then` needs to end with the
next `Future` to run. Add that line, and your code should
compile. Running produces the output:

```
FIXME: use this: ReadDir(ReadDir("."))
```

Cool! Now it's time to look at the [docs for
`ReadDir`](https://docs.rs/tokio-fs/0.1.3/tokio_fs/struct.ReadDir.html). Instead
of a `Future` implementation, this has a `Stream`. Let's shove a
`for_each` in there and see what happens.

__Challenge__ try to guess the errors in the code below before you compile it.

```rust
.and_then(|readdir| {
    readdir
        .for_each(|entry| {
            println!("{:?}", entry.path());
        })
})
```

There are two problems with this code:

1. It leaves an error type of `std::io::Error`
2. It doesn't include `future::ok(())` at the end of the closure
   provided to `for_each`

Go ahead and fix those problems. To be sure we're on the same page,
here's my solution which compiles and runs successfully:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;

fn main() {
    let future = fs::read_dir(".")
        .map_err(|e| eprintln!("Error reading directory: {}", e))
        .and_then(|readdir| {
            readdir
                .map_err(|e| eprintln!("Error reading directory: {}", e))
                .for_each(|entry| {
                    println!("{:?}", entry.path());
                    future::ok(())
                })
        })
        ;
    tokio::run(future)
}
```

### Duplicated error handling

It's a bit irritating that we have two identical `map_err` calls. We
have two different sources of errors: the initial `read_dir` `Future`,
and then streaming the individual `DirEntry`s from it. However, the
type of the errors in both cases is the same:
`std::io::Error`. Therefore, we can move our error handling to the
end, and just do it once:

```rust
fn main() {
    let future = fs::read_dir(".")
        .and_then(|readdir| {
            readdir
                .for_each(|entry| {
                    println!("{:?}", entry.path());
                    future::ok(())
                })
        })
        .map_err(|e| eprintln!("Error reading directory: {}", e))
        ;
    tokio::run(future)
}
```

### Flattening

It turns out that it's common enough to have a `Future` that generates
another `Future`, and then we want to run that second `Future`, that
there's a helper method for it `flatten()`. There's _also_ a
[`flatten_stream()`](https://docs.rs/futures/0.1.23/futures/future/trait.Future.html#method.flatten_stream)
that does the same thing when a `Future` gives us a `Stream`.

__Exercise 8__ Rewrite the code above to use `flatten_stream`. You
should end up with _no calls to `and_then`_. Solution follows
immediately:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;

fn main() {
    let future = fs::read_dir(".")
        .flatten_stream()
        .for_each(|entry| {
            println!("{:?}", entry.path());
            future::ok(())
        })
        .map_err(|e| eprintln!("Error reading directory: {}", e))
        ;
    tokio::run(future)
}
```

### Command line arguments

It's somewhat boring to always print out what's in the current
directory. Instead, let's take all of the command line arguments
(skipping the first, which is the executable name), and list the
directory contents. We'll use `stream::iter_ok` to convert the `Args`
`Iterator` into a `Stream`:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;
use std::env::args;

fn main() {
    let future = stream::iter_ok(args())
        .skip(1)
        .for_each(|dir| {
            fs::read_dir(dir)
                .flatten_stream()
                .for_each(|entry| {
                    println!("{:?}", entry.path());
                    future::ok(())
                })
        })
        .map_err(|e| eprintln!("Error reading directory: {}", e))
        ;
    tokio::run(future)
}
```

Unfortunately, this doesn't compile. The full error message is large
(I encourage you to check it out yourself), but the first few lines
are sufficient to find the problem:

```
error[E0277]: `std::env::Args` cannot be sent between threads safely
  --> src/main.rs:20:5
   |
20 |     tokio::run(future)
   |     ^^^^^^^^^^ `std::env::Args` cannot be sent between threads safely
```

Oh, darn. `Args` isn't thread safe, and so cannot be converted into a
`Stream`. Fair enough: vectors to the rescue!

__Exercise 9__ Create a vector of arguments before the definition of
`future` and use that.

Solution:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;
use std::env::args;

fn main() {
    let args: Vec<String> = args().skip(1).collect();
    let future = stream::iter_ok(args)
        .for_each(|dir| {
            fs::read_dir(dir)
                .flatten_stream()
                .for_each(|entry| {
                    println!("{:?}", entry.path());
                    future::ok(())
                })
        })
        .map_err(|e| eprintln!("Error reading directory: {}", e))
        ;
    tokio::run(future)
}
```

### Where's the concurrency?

If you provide this program two different directories with a large
number of files, you may notice that it processes these directories
sequentially: it will print all of the files in the first directory,
and then all of the files in the second directory. Given that async
I/O and concurrency usually go hand-in-hand, that may be a bit
surprising.

So far, we've only ever had a single task at a time. Our program
streams out the value of `args`, and for each one provides a
`Future`. That `Future` is run to completion, and then the next value
from `args` is processed.

What if we want to process each directory concurrently? To do that, we
need to *spawn* another task, the same way we would spawn a new
thread. Like `tokio::run`, `tokio::spawn` takes a `Future` where both
`Item` and `Error` are `()`. Here's a more concurrent version of our
program:

```rust
let future = stream::iter_ok(args)
    .for_each(|dir| {
        let future = fs::read_dir(dir)
            .flatten_stream()
            .map_err(|e| eprintln!("Error reading directory: {}", e))
            .for_each(|entry| {
                println!("{:?}", entry.path());
                future::ok(())
            })
            ;
        tokio::spawn(future);
        future::ok(())
    })
    ;
```

Notice how I've put `future::ok(())` after the
`tokio::spawn(future);` call. It turns out that's not needed: `spawn`
returns a `Spawn` value, which behaves like `future::ok(())` (via its
`IntoFuture` implementation). So just remove `future::ok` and the
semicolon after `spawn`, and your code will still work.

__NOTE__ You may not notice the concurrency unless you have a large
number of files in each directory.

### Skipping the vector

One final thing that annoyed me above is that `Vec`. It really seems
like we should be able to get away without it. We can't convert `Args`
into a `Stream`, because that would require sending the value between
threads. But now we've got a new trick up our sleeves: spawning. What
if we never send the `Args` anywhere, but just spawn a bunch of tasks.

When I was first learning tokio, I'll admit that I spent way more time
trying to figure out the trick I'm about to show you than I'm proud
of. We need to create a `Future`, then run a `for` loop inside of
it. How do we create a `Future` that lets us run some code without
waiting for anything else? We can use `future::ok(())` to create a
dummy `Future`, and then chain the next action together with
`and_then`.

```rust
let future = future::ok(()).and_then(|()| {
    for dir in args().skip(1) {
        let future = fs::read_dir(dir)
            .flatten_stream()
            .map_err(|e| eprintln!("Error reading directory: {}", e))
            .for_each(|entry| {
                println!("{:?}", entry.path());
                future::ok(())
            })
            ;
        tokio::spawn(future);
    }
    future::ok(())
});
```

Another approach, if you're so inclined, is to use the [`future::poll_fn`](https://docs.rs/tokio/0.1.11/tokio/prelude/future/fn.poll_fn.html) helper function. This takes a 0 argument function which returns a `Result<Async<Item>, Error>`, just like the `poll` method of `Future` does.

__Exercise 10__

Rewrite our program above to use `future::poll_fn`. Your program
should not use `and_then` at all.

Solution:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::fs;
use std::env::args;

fn main() {
    let future = future::poll_fn(|| {
        for dir in args().skip(1) {
            let future = fs::read_dir(dir)
                .flatten_stream()
                .map_err(|e| eprintln!("Error reading directory: {}", e))
                .for_each(|entry| {
                    println!("{:?}", entry.path());
                    future::ok(())
                })
                ;
            tokio::spawn(future);
        }
        Ok(Async::Ready(()))
    });
    tokio::run(future)
}
```

Is all of this ceremony worth it for file system operations? Probably
not. Let's get into something more interesting: network communications!

## TCP client

Rust's standard library already provides really nice support for TCP
communications out of the box. For example, the following code will
print the full response headers and body from an HTTP request to
http://httpbin.org/json:

```rust
use std::io::{Read, Write};
use std::net::TcpStream;

fn main() -> Result<(), Box<std::error::Error>> {
    let mut stream = TcpStream::connect("httpbin.org:80")?;
    stream.write_all(b"GET /json HTTP/1.1\r\nHost: httpbin.org\r\nConnection: close\r\n\r\n")?;

    let mut buffer = vec![];
    stream.read_to_end(&mut buffer)?;

    println!("{}", std::str::from_utf8(&buffer)?);
    Ok(())
}
```

There are some simplifying assumptions here, like using `connection:
close` so that we can use `read_to_end`, and assuming the response
body is properly UTF-8 encoded. But that's not really an indictment of
the TCP support in the standard library.

The real problem is the same one we've been talking about throughout
this lesson: it hogs an entire OS thread blocking on a successful
write to and subsequent read from the network. Let's look at how tokio
can help.

It looks like there's a `TcpStream` type in
[`tokio::net::TcpStream`](https://docs.rs/tokio/0.1.12/tokio/net/struct.TcpStream.html). That
looks like a good place to start. It takes a
[`SocketAddr`](https://doc.rust-lang.org/nightly/std/net/addr/enum.SocketAddr.html),
which we can probably make easily enough. And it returns a
[`ConnectFuture`](https://docs.rs/tokio/0.1.12/tokio/net/tcp/struct.ConnectFuture.html). Let's
start with some code that simple establishes a connection:

```rust
extern crate tokio;

use tokio::net::TcpStream;
use tokio::prelude::*;
use std::net::ToSocketAddrs;

fn main() {
    let mut addr_iter = "httpbin.org:80".to_socket_addrs().unwrap();
    let addr = match addr_iter.next() {
        None => panic!("DNS resolution failed"),
        Some(addr) => addr,
    };
    let future = TcpStream::connect(&addr)
        .map_err(|e| eprintln!("Error connecting: {:?}", e))
        .map(|stream| {
            println!("Got a stream: {:?}", stream);
        });
    tokio::run(future)
}
```

The `to_socket_addrs` business isn't our focus right now, so I'm going
to ignore it. Feel free as an exercise to improve the error handling
of that bit of code.

We've got all of the familiar pieces here: define a `Future`, handle
errors, and use `map` to chain together the action to take with the
open connection.

Let's look a bit more closely at that `stream` value passed to the
closure. It comes from a `ConnectFuture`, so we need to look at the
`Item` associated type there. And sure enough, if you check the docs,
you'll see that it's `TcpStream`. Great.

We used `write_all` in our original, non-async, blocking code. If I
[search for `write_all` in
tokio](https://docs.rs/tokio/0.1.12/tokio/prelude/trait.AsyncWrite.html?search=write_all),
I find that [there's such a helper
function](https://docs.rs/tokio/0.1.12/tokio/io/fn.write_all.html)
which returns a
[`WriteAll`](https://docs.rs/tokio/0.1.12/tokio/io/struct.WriteAll.html)
`Future`. Something interesting:

* `write_all` takes two parameters: an `AsyncWrite` and an
  `AsRef<[u8]>`. This will work out to be our `TcpStream` and the data
  to send.
* The `Item` for `AsyncWrite` is a pair of the variables `A` and `T`,
  which turns out to be exactly the same as the parameters we passed
  in.

Giving us back the stream we originally provided is vital to
continuing the connection. I didn't see any documentation clarifying
the point of returning the byte buffer, but I believe it's returned so
that, if desired, you can reuse a mutable buffer.

Anyway, let's put this together and make our request:

```rust
extern crate tokio;

use tokio::net::TcpStream;
use tokio::io::write_all;
use tokio::prelude::*;
use std::net::ToSocketAddrs;

const REQ_BODY: &[u8] = b"GET /json HTTP/1.1\r\nHost: httpbin.org\r\nConnection: close\r\n\r\n";

fn main() {
    let mut addr_iter = "httpbin.org:80".to_socket_addrs().unwrap();
    let addr = match addr_iter.next() {
        None => panic!("DNS resolution failed"),
        Some(addr) => addr,
    };
    let future = TcpStream::connect(&addr)
        .and_then(|stream| {
            write_all(stream, REQ_BODY)
                .map(|(stream, _body)| println!("Write succeeded: {:?}", stream))
        })
        .map_err(|e| eprintln!("Error occured: {:?}", e))
        ;
    tokio::run(future)
}
```

Notice how I replaced the previous `map` with an `and_then` call, so
that I could provide another `Future` to be performed after the
connection was established.

Would it be too much to ask to _also_ get a `read_to_end` function in
tokio? [Nope, not at
all.](https://docs.rs/tokio/0.1.12/tokio/io/fn.read_to_end.html)

__Exercise 11__ Use `read_to_end` to consume the entire response into
a `Vec<u8>`, and then print that out, using `std::str::from_utf8` and
being as careless with error handling as you like.

Alright, solution:

```rust
extern crate tokio;

use std::net::ToSocketAddrs;
use tokio::io::{read_to_end, write_all};
use tokio::net::TcpStream;
use tokio::prelude::*;

const REQ_BODY: &[u8] = b"GET /json HTTP/1.1\r\nHost: httpbin.org\r\nConnection: close\r\n\r\n";

fn main() {
    let mut addr_iter = "httpbin.org:80".to_socket_addrs().unwrap();
    let addr = match addr_iter.next() {
        None => panic!("DNS resolution failed"),
        Some(addr) => addr,
    };
    let future = TcpStream::connect(&addr)
        .and_then(|stream| {
            write_all(stream, REQ_BODY)
        }).and_then(|(stream, _body)| {
            let buffer = vec![];
            read_to_end(stream, buffer)
        }).map(|(_stream, buffer)| {
            let s = std::str::from_utf8(&buffer).unwrap();
            println!("{}", s);
        }).map_err(|e| eprintln!("Error occured: {:?}", e));
    tokio::run(future)
}
```

## Streaming to a file

I'll repeat for maximum annoyance: tokio is not intended for
asynchronous file operations. That said, there _is_ a
`tokio::fs::File` struct which we can use. Let's try to write the
response contents to `httpbin.json` instead:

```rust
let future = TcpStream::connect(&addr)
    .and_then(|stream| {
        write_all(stream, REQ_BODY)
    }).and_then(|(stream, _body)| {
        let buffer = vec![];
        read_to_end(stream, buffer)
    }).and_then(|(_stream, buffer)| {
        File::create("httpbin.json").and_then(|file| {
            write_all(file, &buffer).map(|_| ())
        })
    }).map_err(|e| eprintln!("Error occured: {:?}", e));
```

Unfortunately, the compiler doesn't like this too much:

```
error[E0277]: the trait bound `std::fs::File: tokio::io::AsyncWrite` is not satisfied
  --> src/main.rs:25:17
   |
25 |                 write_all(file, &buffer).map(|_| ())
   |                 ^^^^^^^^^ the trait `tokio::io::AsyncWrite` is not implemented for `std::fs::File`
   |
   = note: required by `tokio::io::write_all
```

Well, I guess that makes sense: you can't asynchronously write to a
`File`, so `tokio::io::write_all` isn't going to work. Fortunately,
`File` _does_ implement the `Write` trait, which provides a blocking
`write_all`, which is sufficient for our purposes.

__Exercise 12__ Rewrite the code above to successfully write
`httpbin.json`.

First solution, ignoring anything close to proper error handling:

```rust
let future = TcpStream::connect(&addr)
    .and_then(|stream| {
        write_all(stream, REQ_BODY)
    }).and_then(|(stream, _body)| {
        let buffer = vec![];
        read_to_end(stream, buffer)
    }).and_then(|(_stream, buffer)| {
        File::create("httpbin.json").map(|mut file| {
            file.write_all(&buffer).unwrap()
        })
    }).map_err(|e| eprintln!("Error occured: {:?}", e));
```

But ideally, we'd like to avoid that `unwrap()` and instead promote an
I/O error here to be handle by the `map_err` below. It turns out that
there's a surprisingly trivial change to make that happen:

```rust
File::create("httpbin.json").and_then(|mut file| {
    file.write_all(&buffer)
})
```

Instead of using `map`, we use `and_then`, which requires that we
return some value that implements `Future`. But fortunately, `Result`
itself implements `Future`! The `Ok` variant becomes the `Item` for
that `Future`, and the `Err` variant becomes its `Error`. Problem
solved!

## Exercise 13

We haven't taken advantage of tokio here at all! Let's make this
program concurrent. Write a program that takes command line arguments
to determine HTTP requests to make and files to store them to. To
simplify the implementation, we'll have it take input that looks like
the following:

```
$ cargo run httpbin.org:80 /json httpbin.json example.com:80 / homepage.html
```

Feel free to handle invalid command line arguments however's easiest.

### Solution

```rust
extern crate tokio;

use std::net::ToSocketAddrs;
use tokio::io::{read_to_end, write_all};
use tokio::net::TcpStream;
use tokio::prelude::*;
use std::fs::File;

fn download(host: String, path: String, filename: String) -> impl Future<Item=(), Error=()> {
    let mut addr_iter = host.to_socket_addrs().unwrap();
    let addr = match addr_iter.next() {
        None => panic!("DNS resolution failed"),
        Some(addr) => addr,
    };
    let req_body = format!(
        "GET {} HTTP/1.1\r\nHost: {}:80\r\nConnection: close\r\n\r\n",
        path,
        host,
        );

    TcpStream::connect(&addr)
        .and_then(|stream| {
            write_all(stream, req_body).and_then(|(stream, _body)| {
                let buffer = vec![];
                read_to_end(stream, buffer).and_then(|(_stream, buffer)| {
                    File::create(filename).and_then(|mut file| {
                        file.write_all(&buffer)
                    })
                })
            })
        }).map_err(|e| eprintln!("Error occured: {:?}", e))
}

fn main() {
    tokio::run(future::poll_fn(|| {
        let mut args = std::env::args().skip(1);
        loop {
            match (args.next(), args.next(), args.next()) {
                (Some(host), Some(path), Some(filename)) => {
                    tokio::spawn(download(host, path, filename));
                }
                _ => return Ok(Async::Ready(())),
            }
        }
    }))
}
```

## Nicer error handling

We're just `panic!`ing when we have a bad address. Let's do a little
bit better. First, I'll define a helper function to return a nice
`Result`. We'll use a `String` for the `Err` variant, but we could
should ideally define an `enum` instead:

```rust
fn resolve_addr(host: &str) -> Result<SocketAddr, String> {
    let mut addr_iter = match host.to_socket_addrs() {
        Ok(addr_iter) => addr_iter,
        Err(e) => return Err(format!("Invalid host name {:?}: {:?}", host, e)),
    };
    match addr_iter.next() {
        None => Err(format!("No addresses found for host: {:?}", host)),
        Some(addr) => Ok(addr),
    }
}
```

Inside `download`, we could continue `panic!`ing with:

```rust
let addr = resolve_addr(&host).unwrap();
```

But let's do better. Using `?` won't work, since we aren't returning a
`Result`. One idea would be to use `return` to return early:

```rust
let addr = match resolve_addr(&host) {
    Ok(addr) => addr,
    Err(e) => {
        eprintln!("Error resolving address: {}", e);
        return future::err(());
    }
};
```

However, we get an interesting error message from the compiler:

```
error[E0308]: mismatched types
  --> src/main.rs:34:5
   |
34 | /     TcpStream::connect(&addr)
35 | |         .and_then(|stream| {
36 | |             write_all(stream, req_body).and_then(|(stream, _body)| {
37 | |                 let buffer = vec![];
...  |
43 | |             })
44 | |         }).map_err(|e| eprintln!("Error occured: {:?}", e))
   | |___________________________________________________________^ expected struct `tokio::prelude::future::FutureResult`, found struct `tokio::prelude::future::MapErr`
```

In order to make things work, we need to ensure that we always return
the same type. We've so far used `impl Future` to say "we'll return
some type which is a `Future`," but we haven't told the compiler what
that type is. Instead, the compiler has inferred that. But now, we
have two different types.

One approach would be dynamic dispatch, such as using
`Box<Future>`. But there's a better way: using the `Either` helper
type. This type is used in a case where we have two different types of
`Future`s which both have the same `Item` and `Error`. Let's see how
we can rewrite our code above to use `Either`:

```rust
fn download(host: String, path: String, filename: String) -> impl Future<Item=(), Error=()> {
    let addr = match resolve_addr(&host) {
        Ok(addr) => addr,
        Err(e) => {
            eprintln!("Error resolving address: {}", e);
            return future::Either::A(future::err(()));
        }
    };
    let req_body = format!(
        "GET {} HTTP/1.1\r\nHost: {}:80\r\nConnection: close\r\n\r\n",
        path,
        host,
        );

    future::Either::B(TcpStream::connect(&addr)
        .and_then(|stream| {
            write_all(stream, req_body).and_then(|(stream, _body)| {
                let buffer = vec![];
                read_to_end(stream, buffer).and_then(|(_stream, buffer)| {
                    File::create(filename).and_then(|mut file| {
                        file.write_all(&buffer)
                    })
                })
            })
        }).map_err(|e| eprintln!("Error occured: {:?}", e)))
}
```

__Exercise 14__ Implement your own `Either` data type and use it in
the code above.

Solution:

```rust
enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> Future for Either<A, B>
    where A: Future<Item=B::Item, Error=B::Error>,
          B: Future,
{
    type Item = A::Item;
    type Error = A::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self {
            Either::A(a) => a.poll(),
            Either::B(b) => b.poll(),
        }
    }
}
```

## TCP server

Having been so successful with our TCP client, let's move over to the
server side. Conceptually, we want to:

1. Bind a listening socket
2. Accept connections from that socket
3. Copy all data from the input side of the socket to the output side
   of the socket

Binding a listening socket is going to be a [blocking
call](https://docs.rs/tokio/0.1.12/tokio/net/struct.TcpListener.html#method.bind)
to `bind`, taking our old friend `SocketAddr`. Since we're not playing
around with DNS resolution anymore, we can be a bit lazier about how
we do this:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    println!("It worked! {:?}", listener);
}
```

We now have have a `TcpListener`. Unlike other types we've seen, this
doesn't have an implementation of either `Future` or
`Stream`. However, it does have a method called `incoming()`, which
returns an `Incoming`, which has a `Stream` implementation, where
`Item` is `TcpStream`. That looks promising!

```rust
let future = listener
    .incoming()
    .for_each(|socket| {
        println!("Accepted a connection! {:?}", socket);
        future::ok(())
    })
    .map_err(|e| eprintln!("An error occurred: {:?}", e))
    ;
tokio::run(future)
```

And just like that, we've implemented points (1) and (2) above. We're
just left with point 3: copying all of the data. Let's [search tokio
for something to do
copying](https://docs.rs/tokio/0.1.12/tokio/net/struct.TcpListener.html?search=copy). It
looks like
[`tokio::io::copy`](https://docs.rs/tokio/0.1.12/tokio/io/fn.copy.html)
will do. We need to provide it both a reader and writer. Since we're
reader from and writing to the same socket, let's just provide the
same value for both:

```rust
let future = listener
    .incoming()
    .for_each(|socket| {
        copy(socket, socket)
            .map(|_| println!("Connection closed"))
    })
    .map_err(|e| eprintln!("An error occurred: {:?}", e))
    ;
```

Are you already laughing at my comically silly mistake?

```
error[E0382]: use of moved value: `socket`
  --> src/main.rs:13:26
   |
13 |             copy(socket, socket)
   |                  ------  ^^^^^^ value used here after move
   |                  |
   |                  value moved here
```

Of course we can't use the same value in both positions. Fortunately,
when designing `Stream`s, the authors provided a method called
[`split`](https://docs.rs/tokio/0.1.12/tokio/prelude/trait.Stream.html#method.split)
to give us a read and write end of the stream. With that in hand, our
echo server becomes trivial:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;
use tokio::io::copy;

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    let future = listener
        .incoming()
        .for_each(|socket| {
            let (reader, writer) = socket.split();
            copy(reader, writer)
                .map(|_| println!("Connection closed"))
        })
        .map_err(|e| eprintln!("An error occurred: {:?}", e))
        ;
    tokio::run(future)
}
```

## Writing directly

Using `copy` kind of ignores the gory details of what's going on under
the surface. Let's start off by writing some arbitrary message to the
writer side of things, using the [`write_all`
function](https://docs.rs/tokio/0.1.12/tokio/io/fn.write_all.html).

__NOTE__ The `tokio::io::write_all` function takes a `AsyncWrite` and
returns a `WriteAll` `Future`. Don't be confused by the presence of a
`write_all` _method_, which in fact is a blocking call. I wasted about
5 minutes fumbling with that while writing this tutorial.

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;
use tokio::io::{copy, write_all};

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    let future = listener
        .incoming()
        .for_each(|socket| {
            let (reader, writer) = socket.split();
            write_all(writer, b"Welcome to the echo server\r\n")
                .map(|_| println!("Connection closed"))
        })
        .map_err(|e| eprintln!("An error occurred: {:?}", e))
        ;
    tokio::run(future)
}
```

__Exercise 15__ Modify the code above so that, after printing "Welcome
to the echo server", it proceeds to actually echo content sent in.

```rust
write_all(writer, b"Welcome to the echo server\r\n")
    .and_then(|(writer, _)| {
        copy(reader, writer)
            .map(|_| println!("Connection closed"))
    })
```

## Codecs

Reading from the `reader` directly is slightly trickier than writing
to the `writer`. We _could_ go play around with the underlying polling
reading functions, but we're not going to here. (Feel free to [read
the official tokio tutorial for more
information](https://tokio.rs/docs/getting-started/hello-world/).)

Instead, we're going to introduce a new concept, _codecs_. So far,
we've implicitly been working with the `AsyncRead` and `AsyncWrite`
traits, which essentially provide the raw polling functions we'd need
for building up our own `Future`s (as we did way long ago at the
beginning of this lesson). However, we often don't want to work at
that level of abstraction. Instead, we'd like to deal with some kind
of framed (or chunked) data.

The new abstraction instead will be a `Sink`, which is "a value into
which other values can be sent, asynchronously." We'll continue to use
the `Stream` trait for the read side, which we're already quite
familiar with.

Let's contrive an example. Our echo server currently provides slightly
weird output:

```
Hello
Hello
There
There
World
World
```

It's hard to tell what I typed in, and what the server
responded. Instead, I'd like each line sent back from the server to
begin with "You said: ". Doing that with the abstractions we've seen
so far would be fairly painful: we'd need to grab chunks of data, look
for the newline character, break up the input, splice in the "You
said: " message. I know this is a crash course and all, but I'd rather
not crash into that.

Instead, let's jump straight to the better solution. I want to treat
our TCP stream as a stream of lines of data. If I search for the word
"lines" (and this is _actually_ how I learned about codecs), I end up
with
[`LinesCodec`](https://docs.rs/tokio/0.1.12/tokio/codec/struct.LinesCodec.html). It
provides a method `new()`, as well as `new_with_max_length`. We'll use
`new` here, but I recommend reading the docs to see why that's a
terrible idea in any kind of security sensitive context.

The only other method on the type is `max_length`, which doesn't look
like it's going to help us actually deal with a TCP socket as a stream
of lines. So let's look down at the trait implementations. We've got
all of our usual suspects: `Clone`, `PartialOrd`, etc. But two new
ones stick out: `Decoder` and `Encoder`. Well _that_ certainly looks
interesting.

Reading through the docs on `Decoder`, it provides a [method called
`framed`](https://docs.rs/tokio/0.1.12/tokio/codec/trait.Decoder.html#method.framed),
which has a description that is great. (Please, take a second to
follow that link and read the docs.) Without further ado, let's try
adding in our `LinesCodec` to our echo server:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;
use tokio::codec::{Decoder, LinesCodec};

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    let future = listener
        .incoming()
        .for_each(|socket| {
            let lines_codec = LinesCodec::new();
            let socket = lines_codec.framed(socket);
            socket
                .send(String::from("Welcome to the echo server"))
                .map(|_| println!("Connection closed"))
        })
        .map_err(|e| eprintln!("An error occurred: {:?}", e))
        ;
    tokio::run(future)
}
```

You may have noticed that we no longer have a newline sequence at the
end of the "Welcome" string. That's because our lines codec
automatically handles that. Additionally, we now need to use
`String::from`, since the `Item` for this `Sink` is a `String`.

We can also use `split` to isolate the `Sink` from the `Stream`:

```rust
let (sink, stream) = lines_codec.framed(socket).split();
sink
    .send(String::from("Welcome to the echo server"))
    .map(|_| println!("Connection closed"))
```

And, we can use `for_each` on the `Stream` side to get a stream of the
lines:

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;
use tokio::codec::{Decoder, LinesCodec};

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    let future = listener
        .incoming()
        .for_each(|socket| {
            let lines_codec = LinesCodec::new();
            let (sink, stream) = lines_codec.framed(socket).split();
            sink
                .send(String::from("Welcome to the echo server"))
                .and_then(|sink| {
                    stream
                        .for_each(|line| {
                            println!("Received a line: {}", line);
                            future::ok(())
                        })
                        .map(|_| println!("Connection closed"))
                })
        })
        .map_err(|e| eprintln!("An error occurred: {:?}", e))
        ;
    tokio::run(future)
}
```

We're almost done here: we just need to `send` the lines back to the
`sink` instead of to `stdout`. Unfortunately, using the `send` method
we've seen so far is going to be tricky, since we'll end up consuming
the `sink` in each iteration of `for_each`. We could figure out a way
to make that all work, but instead, let's just cut to the chase and
use `send_all`.

__Exercise 16__ Modify the code above so that, instead of printing the
lines to standard output, they get sent back to the client with the
message "You said: ". You'll want to look at
[`send_all`](https://docs.rs/tokio/0.1.12/tokio/prelude/trait.Sink.html#method.send_all)

__Solution__

```rust
extern crate tokio;

use tokio::prelude::*;
use tokio::net::TcpListener;
use tokio::codec::{Decoder, LinesCodec};

fn main() {
    let addr = "127.0.0.1:3000".parse().expect("couldn't parse address");
    let listener = TcpListener::bind(&addr).expect("couldn't bind address");
    let future = listener
        .incoming()
        .for_each(|socket| {
            let lines_codec = LinesCodec::new();
            let (sink, stream) = lines_codec.framed(socket).split();
            sink
                .send(String::from("Welcome to the echo server"))
                .and_then(|sink| {
                    let stream = stream
                        .map(|line| format!("You said: {}", line))
                        ;
                    sink.send_all(stream)
                        .map(|_| println!("Connection closed"))
                })
        })
        .map_err(|e| eprintln!("An error occurred: {:?}", e))
        ;
    tokio::run(future)
}
```

## Next time

Whew, that was a big lesson! At this point, you should have a _very_
solid footing in the ins and outs of tokio. It's time to get lots more
experience with using the library, and related higher level libraries
for doing things like HTTP servers and clients.

Depending on reader feedback, the next lesson may either go deeper
into tokio and related libraries, or go back to more fundamental
aspects of Rust like lifetimes. From the tokio side, we'd play with:

* Message passing between tasks
* UDP communications
* Recursive directory traversal
* Parallel downloading of files

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
