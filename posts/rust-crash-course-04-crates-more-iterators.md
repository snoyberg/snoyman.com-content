I'm getting bored with bouncy, this is our third week talking about this program. It's time to finish this off! How do we do double buffering? It's time to learn about external libraries in Rust, or _crates_.

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

We still haven't fixed our double buffering problem, let's finally do that.
We're also going to introduce some more error handling from
the standard library. And then we'll get to play a bit more with
iterators as well.

## Finding a crate

To do double buffering in the terminal, we're going to want some kind
of a curses library. We're going to look for a crate on
[crates.io](https://crates.io/). On that page, [search for
"curses"](https://crates.io/search?q=curses). Looking through the
download counts and the descriptions,
[pancurses](https://crates.io/crates/pancurses) looks nice, especially
since it supports Unix and Windows. Let's use it!

__Side note__ If you're wondering, this is _exactly_ the process I
went through when writing up bouncy. I had no prior knowledge to tell
me pancurses was going to be the right choice. Also, this program
happens to be the first time I've ever used a curses library.

## Starting a project

We're going to create a new project for this using `cargo new`. We're
going to pull in the bouncy code from the end of week 2 (before the
introduction of command line parsing), since as we'll see later, we
won't need that parsing anymore.

```
$ cargo new bouncy4
$ cd bouncy4
$ curl https://gist.githubusercontent.com/snoyberg/5307d493750d7b48c1c5281961bc31d0/raw/8f467e87f69a197095bda096cbbb71d8d813b1d7/main.rs > src/main.rs
$ cargo run
```

The ball should be bouncing around your terminal, right? Great!

## Adding the crate

The configuration for your project lives in the file `Cargo.toml`,
known as the _manifest_. On my system, it looks like this:

```toml
[package]
name = "bouncy4"
version = "0.1.0"
authors = ["Michael Snoyman <michael@snoyman.com>"]

[dependencies]
```

It will look a bit different on your machine (unless your name is also
Michael Snoyman). Notice that `[dependencies]` section at the end?
That's where we add information on external crates. If you go back to
[the pancurses page on
crates.io](https://crates.io/crates/pancurses), you'll see:

```
Cargo.toml  pancurses = "0.16.0"
```

It may be different when you're reading this. That's telling us what
we can add to the dependencies section to depend on the library. There
are lots of details about how to specify dependencies, which we won't
get into here (and probably never will in the crash course, I've spent
enough of my life discussing dependency management already :) ). You
can read more in [the Cargo book
reference](https://doc.rust-lang.org/cargo/reference/manifest.html).

Anyway, go ahead and add `pancurses = "0.16.0"` to the end of your
`Cargo.toml` and run `cargo build`. `cargo` should run off and do some
updating, downloading, compiling, and end up with an executable that
does exactly the same thing as it did before. Perfect, time to use it!

__NOTE__ It seems like, at the time of writing, `pancurses` does not build
against the latest version of the `ncurses` package. If you get a build
failure, you may need to also add `ncurses = "= 5.94.0"` to your `dependencies`
to force Cargo to use an older, compatible version.

With the current version of the Rust, you'll also need to add the following to
the top of `src/main.rs`:

```rust
extern crate pancurses;
```

This requirement is disappearing with Rust 2018. If you'd like play around with
that, you'll need to switch to a nightly compiler and add `edition = "2018"` to
your `Cargo.toml`. But we're going to stick to the stable compiler and Rust
2015.

## Library docs

On the crates.io page, there's a [link to the
documentation](https://docs.rs/pancurses/0.16.0/pancurses/) for
pancurses. Open that in a new tab. Also, reading down the crates page,
we get a nice usage example. In `main()`, the first call is to
`initscr`. Jump over to the API documentation and search for
`initscr`, and you'll end up at [the `initscr`
function](https://docs.rs/pancurses/0.16.0/pancurses/fn.initscr.html).

Let's try this out. Add the following line to the top of your `main`
function and compile:

```rust
let window = pancurses::initscr();
```

Great! We're going to use that returned `Window` struct to interact
with pancurses. Go ahead and [open its
documentation](https://docs.rs/pancurses/0.16.0/pancurses/struct.Window.html)
and start browsing through.

## Getting the frame size

We're currently just assigning an arbitrary frame size. Ideally, we'd
like to base it off of the actual terminal size. `Window` provides a
method for this,
[`get_max_yx`](https://docs.rs/pancurses/0.16.0/pancurses/struct.Window.html#method.get_max_yx).

First, a step for you dear reader: modify the `new` method of `Game`
to take a `Frame` as input. Then, let's jump back to our `main`. We
can capture the maximum `y` and `x` values with:

```rust
let (max_y, max_x) = window.get_max_yx();
```

And now let's create a `Frame` value out of those.

```rust
let frame = Frame {
    width: max_x,
    height: max_y,
};
```

Then pass that value into `Game::new()`. This will only work if you already
modified `new` to take an extra `Frame` argument, go back a few paragraphs if
you missed that.

__Challenge__ Before you hit compile, can you figure out what error
message we're about to encounter?

When I run `cargo build`, I get the following error:

```
error[E0308]: mismatched types
   --> src/main.rs:109:16
    |
109 |         width: max_x,
    |                ^^^^^ expected u32, found i32

error[E0308]: mismatched types
   --> src/main.rs:110:17
    |
110 |         height: max_y,
    |                 ^^^^^ expected u32, found i32
```

If you remember, `width` and `height` are `u32`s, but pancurses gives
us `i32`s for the maximum x and y values. How do we convert? One easy
way to handle this is with `as u32`:

```rust
width: max_x as u32,
height: max_y as u32,
```

This is a totally unchecked cast. To demonstrate, try running this
program:

```rust
fn main() {
    for i in -5i32..6i32 {
        println!("{} -> {}", i, i as u32)
    }
}
```

(Yes, there's syntax in Rust for following a number with its type like
that, it's really nice.)

Which results in:

```
-5 -> 4294967291
-4 -> 4294967292
-3 -> 4294967293
-2 -> 4294967294
-1 -> 4294967295
0 -> 0
1 -> 1
2 -> 2
3 -> 3
4 -> 4
5 -> 5
```

We're going to just accept this bad behavior for now. Don't worry,
it'll get worse.

## Carriage return and the border

If you run this program, you'll get some really weird looking
output. If you don't believe me, go run it yourself now. I'll wait.

The first problem is that our newlines aren't working as expected. We
previously used `\n` to create a newline. However, with curses
enabled, this create a _line feed_, moving the cursor down one row,
without a _carriage return_, moving the cursor to the beginning of the
line. Go ahead and replace the `\n` usages with `\r\n`.

That's better, but there's still a problem: the grid doesn't fit in
the terminal! That's because we didn't take the size of the border
into account. Try subtracing 4 from the maximum x and y values and see
if that fixes things. (Note: there's a bit of a trick to where you put
the `- 4`. Think about what value you're trying to cast.)

## Double buffering

We still haven't done double buffering, but we're in a much better
position to do so now! The trick is to replace our `println!` call in
the `main` function's `loop` with calls to `window` methods. If you
want the challenge, go read through the docs and try to figure out how
to make it work. One hint: you'll need to revert the addition of the
`\r` carriage returns we added above.

```rust
loop {
    window.clear(); // get rid of old content
    window.printw(game.to_string()); // write to the buffer
    window.refresh(); // update the screen
    game.step();
    std::thread::sleep(sleep_duration);
}
```

Hurrah, we have double buffering in place! We can finally be done with
these bouncing balls.

Or can we?

## Exercise 1

It's time to get more comfortable with exploring API docs, and writing
some Rust code with less training wheels. There are a few problems
with our implementation:

* We don't need to generate an entire string for the whole
  grid. Instead, we can use some `Window` methods for moving the
  cursor around and adding characters. Use that instead.
* Drawing the border as we have is harder than it should be. There's a
  method on `Window` that will help significantly.
* We have a number of assumptions about numbers, in particular that
  the maximum x and y are positive, and large enough to hold the
  ball's starting position. Put in some sane limits: both values must
  be at least 10.
* More advanced, but: pancurses has some built in support for input
  handling with timeouts. Instead of using `std::thread::sleep`, we
  can set a timeout on input handling and add that to our main
  loop. You can then also respond to two specific kinds of input:
    * Exit the program on a `q` press
    * Reset the game when the size of the window changes

## More iterators!

Alright, I'm sufficiently bored with bouncing balls. Let's talk about
something far more interesting: streaming data. Personally I found it
easiest to understand iterators by writing a few of them myself, so
that's where we're going to start.

Let's do some compiler-guided programming. We discussed previously
that there's an `Iterator` trait. So a fair bet is that we need to
create a new data type and provide an implementation of the `Iterator`
trait. Let's start off with something simple: an iterator that
produces no values at all.

```rust
struct Empty;

fn main() {
    for i in Empty {
        panic!("Wait, this shouldn't happen!");
    }
    println!("All done!");
}
```

`panic!`ing is a way to cause the current thread to exit due to an
impossible situation. It's _kind of_ like runtime exceptions in other
languages, except you can't recover from them. They are only
intended to be used for impossible situations.

OK, compile that and you'll get a helpful error message:

```
error[E0277]: the trait bound `Empty: std::iter::Iterator` is not satisfied
```

Cool, let's add an empty implementation (no pun intended):

```rust
impl Iterator for Empty {
}
```

More help from the compiler!

```
error[E0046]: not all trait items implemented, missing: `Item`, `next`
 --> foo.rs:3:1
  |
3 | impl Iterator for Empty {
  | ^^^^^^^^^^^^^^^^^^^^^^^ missing `Item`, `next` in implementation
  |
  = note: `Item` from trait: `type Item;`
  = note: `next` from trait: `fn(&mut Self) -> std::option::Option<<Self as std::iter::Iterator>::Item>`
```

So we need to provide two things: `Item` and `next`. `next` is a
function, so we'll get to that in a second. What about that `type
Item;`? It's what we call an _associated type_. It tells us what type
of values will be produced by this iterator. Since we're not producing
anything, we can use any type here. I'll use a `u32`:

```rust
type Item = u32;
```

Now we need to add in a `next`. Above, it gives the type of that
function as:

```
fn(&mut Self) -> std::option::Option<<Self as std::iter::Iterator>::Item>
```

Let's simplify this bit by bit. The _type_ of the input is `&mut
Self`. However, the correct syntax will be `&mut self`. Find that
confusing? Remember that `&mut self` is a shortcut for `self: &mut
Self`.

```
fn(&mut self) -> std::option::Option<<Self as std::iter::Iterator>::Item>
```

Next, we can remove the module qualifiers for `Option` and `Iterator`,
since they're already in our namespace:

```
fn(&mut self) -> Option<<Self as Iterator>::Item>
```

This `Self as Iterator` is interesting. It's saying "take the current
type, and look at its `Iterator` implementation. The reason we care
about specifying the implementation is because of what comes next:
`::Item`. What we're _really_ saying is "we want the `Item` associated
type related to the `Iterator` implementation." It's possible that
other traits will _also_ have an associated type with the same name,
so this is an unambiguous way to refer to it.

Anyway, let's see if this type signature works. Include the name of
the function and give it a dummy function body:

```rust
fn next(&mut self) -> Option<<Self as Iterator>::Item> {
    unimplemented!()
}
```

`unimplemented!()` is a macro that uses `panic!` under the surface,
and is a convenient way to stub out implementations while under active
development. If you compile this, it will succeed. Yay! Then it
crashes at runtime due to the `unimplemented!`. Cool.

We can simplify the signature a bit by removing the `as Iterator`,
which isn't necessary:

```rust
fn next(&mut self) -> Option<Self::Item>
```

You can _also_ replace the `Self::Item` directly with `u32` if you
want. The upside is, in this case, it's shorter. The downside is that
if you change the `Item` in the future, you'll have to change it in
two places. This is really a subjective point, your call.

Alright, let's provide an implementation. We return an `Option`, which
is an `enum` with two variants: `None` or `Some`. The former means "we
don't have anything," the latter means "we have something." Given that
we're implementing the empty iterator, returning `None` seems like the
right move:

```rust
fn next(&mut self) -> Option<u32> {
    None
}
```

And just like that, we have our first `Iterator` implementation!

## Exercise 2

Create an iterator that infinitely produces the number 42. Here's a
`main` function to test it out:

```rust
fn main() {
    // only take 10 to avoid looping forever
    for i in TheAnswer.take(10) {
        println!("The answer to life, the universe, and everything is {}", i);
    }
    println!("All done!");
}
```

## Mutable state

The signature for `next` involves taking a mutable reference to
`self`. Let's use it! We're going to create an iterator that counts
from 1 to 10. (If you're feeling brave, try to implement it yourself
before reading my solution.)

```rust
struct OneToTen(u32);

fn one_to_ten() -> OneToTen {
    OneToTen(1)
}

impl Iterator for OneToTen {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        if self.0 > 10 {
            None
        } else {
            let res = Some(self.0);
            self.0 += 1;
            res
        }
    }
}

fn main() {
    for i in one_to_ten() {
        println!("{}", i);
    }
}
```

__Exercise 3__ Create an iterator that produces the Fibonacci
sequence. (Anyone who's heard me bemoaning this exercise in the
functional programming world should have a hearty chuckle right now.)

## Iterator adapters

We can also write an iterator that will modify an existing
iterator. Let's write `Doubler`, which will double the values produced
by a previous iterator. In order to make this work, we're going to
capture the underlying iterator inside our new data type, which will
also necessitate parameterizing our `Doubler` data type on the
contained iterator:

```rust
struct Doubler<I> {
    iter: I,
}
```

Let's throw in a `main` function to show how this is used:

```rust
fn main() {
    let orig_iter = 1..11; // array indices start at 1
    let doubled_iter = Doubler {
        iter: orig_iter,
    };
    for i in doubled_iter {
        println!("{}", i);
    }
}
```

Great. If we compile this, we get an error about the missing
`Iterator` implementation. Let's try to write something for this:

```rust
impl Iterator for Doubler {
}
```

When we compile this, we get the error message:

```
error[E0107]: wrong number of type arguments: expected 1, found 0
 --> foo.rs:5:19
  |
5 | impl Iterator for Doubler {
  |                   ^^^^^^^ expected 1 type argument
```

OK, that makes sense. `Doubler` itself isn't a type until we give it
its parameters. Let's do that:

```rust
impl Iterator for Doubler<I> {
}
```

We get two error messages. Feel free to look at the second if you
like, but it's not terribly helpful. (Recommendation: always look at
the first error message first and try to solve that before moving on.)
The first error message is:

```
error[E0412]: cannot find type `I` in this scope
 --> foo.rs:5:27
  |
5 | impl Iterator for Doubler<I> {
  |                           ^ not found in this scope
```

OK, what's happening? When we provide an implementation, we need to
state all of the type variables we want upfront. So let's do this:

```rust
impl<I> Iterator for Doubler<I> {
}
```

That may look a bit redundant (it did to me at first), but eventually
you'll get to cases where things are more complicated and the two sets
of angle brackets don't look identical. (For Haskellers or PureScript
users, this is kind of like requiring an explicit `forall`.)

Alright, now we've got something closer, and the compiler is upset
that we haven't given `type Item` and `next`. Let's go ahead and
return a `u32` again:

```rust
type Item = u32;
fn next(&mut self) -> Option<u32> {
    unimplemented!()
}
```

The compiles and runs, and then crashes because of the
`unimplemented!`. Great, progress! The trick here is we want to ask
the underlying `Iterator` for its next value. We'll do this with some
explicit pattern matching (functional programmers: yes, there's a
`map` method on `Option` we could use here).

```rust
fn next(&mut self) -> Option<u32> {
    match self.iter.next() {
        None => None,
        Some(x) => Some(x * 2),
    }
}
```

Nice enough, but when we compile it, we get told:

```
error[E0599]: no method named `next` found for type `I` in the current scope
 --> foo.rs:8:25
  |
8 |         match self.iter.next() {
  |                         ^^^^
  |
  = help: items from traits can only be used if the trait is implemented and in scope
  = note: the following traits define an item `next`, perhaps you need to implement one of them:
          candidate #1: `std::iter::Iterator`
          candidate #2: `std::str::pattern::Searcher`
```

The compiler knows that we _might_ mean the `next` method from
`Iterator`. But it doesn't use it. Why, you might ask? Because _we
never told the compiler that the implementation exists_! We need to
indicate that the `I` parameter must have an `Iterator`
implementation.

```rust
impl<I: Iterator> Iterator for Doubler<I>
```

That's some new syntax, but pretty straightforward: `I` must have an
implementation of `Iterator`. Unfortunately, we're not out of the
woods quite yet:

```
error[E0369]: binary operation `*` cannot be applied to type `<I as std::iter::Iterator>::Item`
  --> foo.rs:10:29
   |
10 |             Some(x) => Some(x * 2),
   |                             ^^^^^
   |
   = note: an implementation of `std::ops::Mul` might be missing for `<I as std::iter::Iterator>::Item`
```

Let's talk this through. `I` is some `Iterator`, we've already
established that. And we know that the `x` value we use in `x * 2`
will be of whatever type the `Item` associated type for that `I`
is. The problem is: we have no idea what it is, or whether or not it
can be multiplied!

We've already said we're going to produce `u32`s here, so can we just
enforce that the `Item` is a `u32`? Yes!

```rust
impl<I: Iterator<Item=u32>> Iterator for Doubler<I>
```

Whew, our code works!

## Alternative syntax: where

As our constraints get more complex, shoving them all into the
parameters at the beginning of `impl` starts to feel crowded. You can
alternatively use `where` for this:

```rust
impl<I> Iterator for Doubler<I>
    where
    I: Iterator<Item=u32>
```

There's a subjective point at which people decide to make that
transition. Personally, I prefer consistency over brevity, and almost
always use `where`. Your mileage may vary. You should be aware of both
syntaxes for reading other people's code.

## Not just u32

It's a bit weird that we're tied down to `u32`s. What if we change our
`main` funciton to have:

```rust
let orig_iter = 1..11u64;
```

We'll get a compiler error:

```
error[E0271]: type mismatch resolving `<std::ops::Range<u64> as std::iter::Iterator>::Item == u32`
  --> foo.rs:23:14
   |
23 |     for i in doubled_iter {
   |              ^^^^^^^^^^^^ expected u64, found u32
   |
   = note: expected type `u64`
              found type `u32`
```

It's possible to relax this, but it starts to get more
complicated. But let's do it! We'll need to remove all of the
references to `u32` in our implementation. Here's a first stab at
this:

```rust
impl<I> Iterator for Doubler<I>
    where
    I: Iterator
{
    type Item = ???;
    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(x) => Some(x * 2),
        }
    }
}
```

I've replaced `Option<u32>` with `Option<Self::Item>`, and removed the
`<Item = u32>` on the `I: Iterator`. But what should we use for `type
Item =`? We want it to be the same type as the underlying iterator's
`Item`... so let's just say that!

```rust
type Item = I::Item;
```

That works! We're still not compiling though, because Rust doesn't
know that it can perform multiplication on `I::Item`. Fortunately,
there's a trait for things that can be multiplied called `Mul`. We can
add in:

```rust
where
I: Iterator,
I::Item: std::ops::Mul,
```

New error message:

```
error[E0308]: mismatched types
  --> foo.rs:14:29
   |
14 |             Some(x) => Some(x * From::from(2u8)),
   |                             ^^^^^^^^^^^^^^^^^^^ expected std::iter::Iterator::Item, found std::ops::Mul::Output
   |
   = note: expected type `<I as std::iter::Iterator>::Item`
              found type `<<I as std::iter::Iterator>::Item as std::ops::Mul>::Output`
```

It turns out that `Mul` has an associated type for its output. This is
useful for expressing more complicated relationships at the type
level. For example, we can define types for `Force`, `Mass`, and
`Acceleration`, and then define a `Mul` implementation that says
`Mass` times `Acceleration` produces a `Force`.

That's a wonderful feature, but it's getting in our way here. We want
to say "the output should be the same as the item itself." Fair
enough:

```rust
impl<I> Iterator for Doubler<I>
    where
    I: Iterator,
    I::Item: std::ops::Mul<Output=I::Item>,
```

And now:

```
error[E0308]: mismatched types
  --> foo.rs:14:33
   |
14 |             Some(x) => Some(x * 2),
   |                                 ^ expected associated type, found integral variable
   |
   = note: expected type `<I as std::iter::Iterator>::Item`
              found type `{integer}`
```

Ugh. We have `2`, which can be _some_ integral type. But we don't know
that `Item` is an integral type! I'm not aware of a way to give a
constraint that allows this code to work (if someone knows better,
please let me know and I'll update this text). One trick that _does_
work is to upcast from a `u8` using the `From` trait, which performs
safe numeric conversions (which cannot over or underflow):

```rust
impl<I> Iterator for Doubler<I>
    where
    I: Iterator,
    I::Item: std::ops::Mul<Output=I::Item> + From<u8>,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(x) => Some(x * From::from(2u8)),
        }
    }
}
```

Whew, that's finally over!

__Exercise 4__ An easier approach to the above is to use `x + x`
instead of `x * 2`. Rewrite the iterator to do that. One hint: the
compiler won't know that it's allowed to make copies of that type
unless you tell it via the appropriately-named trait.

## Recap

That was a lot! But hopefully it gets the idea across of how iterators
work. We can write highly generic adapters that work with many kinds
of input. You could apply `Doubler` to the range iterator as we
did. You could apply it to the `Empty` we defined earlier. Or to
dozens of other things.

You may notice that the types of these iterators seem to grow as you
add more things to them. That's absolutely true, and it's also by
design. By representing the full type statically, the compiler is able
to see all of the actions that are being performed in a pipeline of
iterator operations, and optimize things very well.

## More idiomatic

The `Doubler` we wrote was not idiomatic at all. Let's do it the
_real_ way:

```rust
fn main() {
    for i in (1..11).map(|x| x * 2) {
        println!("{}", i);
    }
}
```

The `Iterator` trait includes many helper methods, so you can chain up
large sets of actions like that:

```rust
fn main() {
    for i in (1..11).skip(3).map(|x| x + 1).filter(|x| x % 2 == 0) {
        println!("{}", i);
    }
}
```

You could of course write something like this as a for loop in C/C++,
but:

* It would be harder to see the logic
* It would be harder to extend in the future
* It wouldn't be faster: the Rust compiler will optimize case like
  this down to the same hand-rolled loop you may write

## Collecting results

You can collect the results from an iterator in a vector:

```rust
fn main() {
    let my_vec: Vec<u32> = (1..11).collect();
    println!("{:?}", my_vec);
}
```

The type annotation is necessary here, since `collect` can work on
many different datatypes.

__Exercise 5__ Use the `fold` method to get the sum of the numbers
from 1 to 10. Extra credit: write a helper `sum` function.

## Next time

We've been dancing around closures for quite a while now, including in
that last exercise. We now know enough to attack them head on. We'll
do so next time.

You're now at a point where you can write some real Rust applications
and start cutting your teeth. I'd recommend finding a few play tasks
you want to experiment with. [Exercism](https://exercism.io) may be a
good choice if you're looking for some challenges.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
