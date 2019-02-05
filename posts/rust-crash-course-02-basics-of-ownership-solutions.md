Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Basics of Ownership."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

Implementing `drop` is fairly simple. We need a function that allows a
value to be moved into it, and then does nothing with that value.

```rust
fn drop<T>(_: T) {
}
```

And surprise: check out [the actual `drop`
function](https://github.com/rust-lang/rust/blob/8bf7fda6b5fec99a11ae0fb8d5c3dbd150063741/src/libcore/mem.rs#L776).

## Exercise 2

The important trick to implement this method syntax is that the first
parameter _must_ be some form of `self`. We want to keep this as an
immutable reference, so we use `&self`.

```rust
#[derive(Debug)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) {
        println!("Dropping a Foobar: {:?}", self);
    }
}

impl Foobar {
    fn use_it(&self) {
        println!("I consumed a Foobar: {:?}", self);
    }
}

fn main() {
    let x = Foobar(1);
    println!("Before uses_foobar");
    x.use_it();
    x.use_it();
    println!("After uses_foobar");
}
```

You may be wondering: why does the code use `x.use_it()`? `use_it`
requires a _reference_ to a `Foobar`, but `x` is a `Foobar`! In fact,
you may have ended up writing something like this:

```rust
fn main() {
    let x = Foobar(1);
    println!("Before uses_foobar");
    (&x).use_it();
    (&x).use_it();
    println!("After uses_foobar");
}
```

While that's perfectly valid code, it's also unnecessary: Rust will
automatically take a reference to a value in the case of a method
call.

## Exercise 3

The original code doesn't compile, since our `x` value was
moved. However, if we have a `Copy` implementation, Rust will
automatically create a copy of the value for us. As I hinted, you can
do this easily by using automatic deriving:

```rust
#[derive(Debug, Clone, Copy)]
struct Foobar(i32);

fn uses_foobar(foobar: Foobar) {
    println!("I consumed a Foobar: {:?}", foobar);
}

fn main() {
    let x = Foobar(1);
    uses_foobar(x);
    uses_foobar(x);
}
```

If you want to be more explicit, you can write the implementations
directly. Note that, to the best of my knowledge, there's no advantage
to doing so, at least in this case:

```rust
impl Clone for Foobar {
    fn clone(&self) -> Self {
        Foobar(self.0)
    }
}
impl Copy for Foobar {
}
```

There's no need for a body for the `Copy` trait. Its only purpose is
as a signal to the compiler that it's acceptable to copy when needed.

## Exercise 4

We're not taking a reference to `x` when calling `double`, so our
`double` function needs to take an actual `Foobar`, not a reference to
one. It must also return a `Foobar`. One implementation we could come
up with is:

```rust
fn double(foobar: Foobar) -> Foobar {
    Foobar(foobar.0 * 2)
}
```

This takes in an immutable `Foobar`, and then constructs a new
`Foobar` from the value inside of it. Another option, however, would
be to mutate the original `Foobar` and then return it:

```rust
fn double(mut foobar: Foobar) -> Foobar {
    foobar.0 *= 2;
    foobar
}
```

The Haskeller in me prefers the first implementation, since it has
less mutation. The troll in me loves the second one, since it lets me
taunt the Haskeller in me.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
