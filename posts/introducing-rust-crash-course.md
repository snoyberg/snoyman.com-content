I'm announcing an upcoming series of blog posts, which I'm calling the
Rust crash course. In this blog post, I want to explain:

* Why I'm interested in Rust
* Why I'm writing up this series
* Who this series is for
* My intended flavor for the series
* Some examples of Rust gotchas I want to cover

I'm getting to work on this series due to [increased Rust usage at FP Complete](https://www.fpcomplete.com/rust).

## Why Rust?

I'm a strong believer in using the compiler to help eliminate bugs. No
programming language can eliminate all bugs and even the best
designed language will typically need to leave developers plenty of
wiggle room to shoot themselves in the foot. Still, there's
significant value in safety and long term maintainability of projects
that use languages with this focus.

For about ten years now, my primary language has been (and continues
to be) Haskell. It gets a lot of things right here, such as
immutability, explicit effects, and strong typing. But I've got two
problems with an intense focus on Haskell:

* There's always something to be learned from other languages. It's
  quite easy to fall into the trap of knowing one language well, and
  becoming blind to its shortcomings. Haskell is now the language I've
  used for the longest stretch of time as a primary language, finally
  displacing Java. I need to avoid this trap.
* There are some legitimate technical advantages of Rust:
    * Performance is better, and more reliable as well, relying less
      upon things like rewrite rules firing
    * It's got a better story right now for mobile and frontend
      development
    * Lack of garbage collection opens up new possibilities in real
      time development
    * Rust improves on Haskell's safety guarantees in some places,
      such as defaulting to requiring complete pattern matches

Like many others, I've been hearing a buzz around Rust for years. Two
or so years ago, I started playing around with it more, and have been
steadily dedicating more personal time to it. But recently, we've had
more [Rust interest at work](https://www.fpcomplete.com/rust), and therefore we've been
expanding our internal Rust team.

## Why this series?

We're a globally distributed team at FP Complete, and we make a heavy
push towards using written communication tools wherever possible. This
also overlaps heavily with training material. As I do more training
(both internally, and for customers), I'm discovering places where:

* I'm finding my own knowledge of the language was lacking
* Newcomers are stumbling hard

This series is intended to collect both initial pointers of how to get
started with Rust, and hard-learned lessons of how to avoid getting
stuck. Some of these stumbling blocks may favor the kind of audience
I'm working with directly (lots of Haskell and DevOps engineers), but
that will likely change over time.

## Target audience

I'm gearing this series towards the Rust curious. I'm assuming programming
knowledge, and some basic idea of what Rust is about, but no real knowledge of
the language itself. I'll try to call out when you should go read the Rust
book.

If you're a Java user, a Rubyist, or a Haskeller, and Rust intrigues you, I
hope this will help you. And maybe even Rustaceans will enjoy seeing the pain
points I find myself and others are hitting.

## Flavor of the series

There is already [really good
material](https://www.rust-lang.org/en-US/documentation.html) on Rust
available from rust-lang.org. I have no intention of trying to replace
that. Instead, I'll assume that people are reading through [the Rust
book](https://doc.rust-lang.org/book/), and point to sections where
appropriate.

One concrete example: I don't intend to spend a lot of time talking
about Rust syntax, or explaining that it's an expression-oriented
language. The book covers that.

Instead, I want to give people:

* Real code to look at and play with
* Simple problems to build up experience with
* Explanations of tricky cases that catch people up

And on that note...

## Rust gotchas

A few people on Twitter asked me to share some Rust gotchas,
especially coming from the perspective of a Haskell developer. I'll
certainly be covering more gotchas going forward, but I wanted to give
some examples in this first post so you can get a feel for the kinds
of things we'll be addressing in the series. I'm _not_ going to be
explaining details of these problems here; that's what the series is
for!

Just so you know: there's no content following these examples, besides
the Disqus comments below. If you're not interested in the gotchas,
feel free to quit reading now and [stay
tuned](/feed/rust-crash-course) for more posts.

### Mutable values, mutable variables

I've got a simple mental model in Haskell. Values are all immutable,
period. A few special reference types allow me to mutate their
contents. And mutating like that is an effect tracked in the type
system.

From that perspective, Rust is a bit surprising. Here's one:

```rust
fn main() {
    let i: isize = 1;
    let j: isize = foo(i);
    println!("{}", j);
}

fn foo(mut i: isize) -> isize {
    i += 1;
    i
}
```

Wait a second... `i` is immutable. Then I pass it to `foo`, and it
becomes mutable. Then I return this mutable value as an immutable
value. _What?_

I assure you, this ultimately makes sense, but it's kind of
surprising. Also, the fact that `x: &mut isize` and `mut x: &mut
isize` are both real things that mean different things:

```rust
fn main() {
    let mut i: isize = 1;
    let mut j: isize = 2;
    foo(&mut i, &mut j);
    println!("{} {}", i, j);
}

fn foo<'a>(mut i: &'a mut isize, j: &'a mut isize) {
    *i *= 10;
    i = j;
    *i *= 10;
}
```

### So many strings

I was warned about this one and blew it off. I thought to myself
there's no way a Haskeller, trained in the arts of `String`, strict
`Text`, lazy `Text`, `ByteString`s and more could be daunted. I was
wrong.

```rust
fn main() {
    let hello1 = String::from("Hello, ");
    let hello2 = String::from(", hello!");
    let name = "Alice";
    println!("{}", hello1 + name);
    println!("{}", name + hello2);
}
```

Nope, the code above doesn't compile.

### Magically works, until it doesn't

There are a number of "magic" things that happen in Rust in the name
of ergonomics. Often, they work perfectly and save a lot of
frustration. And sometimes, they fail. Look at this broken code:

```rust
fn main() {
    for arg in std::env::args().skip(1) {
        respond(arg);
    }
}

fn respond(arg: &str) {
    match arg {
        "hi" => println!("Hello there!"),
        "bye" => println!("OK, goodbye!"),
        _ => println!("Sorry, I don't know what {} means", arg),
    }
}
```

You'll get an error message:

```
expected &str, found struct `std::string::String`
```

Oh, well that makes sense! I need to get a reference to a `str` instead of the `String` I got from `args()`. Easy enough to fix:

```rust
respond(&arg);
```

But then I realize that the respond function is silly and inline the
`match`:

```rust
fn main() {
    for arg in std::env::args().skip(1) {
        match &arg {
            "hi" => println!("Hello there!"),
            "bye" => println!("OK, goodbye!"),
            _ => println!("Sorry, I don't know what {} means", arg),
        }
    }
}
```

I remembered to match on `&arg` instead of `arg`, so you'd think it
would be fine. But it isn't:

```
  |             ^^^^ expected struct `std::string::String`, found str
  |
  = note: expected type `&std::string::String`
             found type `&'static str`
```

Huh, that's weird. In order to figure out what's going on here, you
have to understand quite a few details of the deref magic going on
behind the scenes. I dare say some of this magic is even a leaky
abstraction. (Don't worry, it's still zero cost.) You can solve this
with either:

```rust
match &*arg {
```

or

```rust
match arg.as_ref() {
```

### Moving data into closures

The biggest pain points I've encountered so far in my Rust odyssey are
all around moving data into closures. I've been spoiled by Haskell: I
like to use closures constantly, and am used to garbage collection
just letting it all magically work.

I've got some _real_ head scratchers to demonstrate later, but have
fun with this relatively simple example:

```rust
fn main() {
    let hello = String::from("Hello, ");
    let greet = |name| hello + name;
    println!("{}", greet("Alice"));
    println!("{}", greet("Bob"));
}
```
