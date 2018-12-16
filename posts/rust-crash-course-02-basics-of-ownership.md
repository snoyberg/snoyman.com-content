Arguably the biggest distinguishing aspect of Rust versus other
popular programming languages is its ownership model. In this lesson,
we're going to hit the basics of ownership in Rust. You can read much
more in the [Rust book chapter on
ownership](https://doc.rust-lang.org/book/second-edition/ch04-01-what-is-ownership.html).

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Format

I'm going to be experimenting a bit with lesson format. I want to
cover both:

* More theoretical discussions of ownership
* Trying to implement an actual program

As time goes on in this series, I intend to spend more time on the
latter and less on the former, though we still need significant time
on the former right now. I'm going to try approaching this by having
the beginning of this post discuss ownership, and then we'll implement
a first version of bouncy afterwards.

This format may feel a bit stilted, feedback appreciated if this
approach works for people.

## Comparison with Haskell

I'm going to start by comparing Rust with Haskell, since both
languages have a strong concept of immutability. However, Haskell is a
garbage collected language. Let's see how these two languages
compare. In Haskell:

* Everything is immutable by default
* You use explicit mutability wrappers (like `IORef` or `MVar`) to
  mark mutability
* References to data can be shared however much you like
* Garbage collection frees up memory non-deterministically
* When you need deterministic resource handling (like file handles),
  you need to use [the bracket
  pattern](https://www.snoyman.com/blog/2018/10/raii-better-than-bracket-pattern)
  or similar

In Rust, data ownership is far more important: it's a primary aspect
of the language, and allows the language to bypass garbage
collection. It also allows data to often live on the stack instead of
the heap, leading to better performance. Also, it runs
deterministically, making it a good approach for handling other
resources like file handles.

Ownership starts off with the following rules:

* Each value in Rust has a variable that’s called its owner.
* There can only be one owner at a time.
* When the owner goes out of scope, the value will be dropped.

## Simple example

Consider this code:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn uses_foobar(foobar: Foobar) {
    println!("I consumed a Foobar: {:?}", foobar);
}

fn main() {
    let x = Foobar(1);
    uses_foobar(x);
}
```

__Syntax note__ `#[...]` is a compiler pragma. `#[derive(...)]` is one
example, and is similar to using `deriving` for typeclasses in
Haskell. For some traits, the compiler can automatically provide an
implementation if you ask it.

__Syntax note__ `{:?}` in a format string means "use the `Debug` trait
for displaying this"

`Foobar` is what's known as a newtype wrapper: it's a new data type
wrapping around, and with the same runtime representation of, a signed
32-bit integer (`i32`).

In the `main` function, `x` contains a `Foobar`. When it calls
`uses_foobar`, ownership of that `Foobar` passes to
`uses_foobar`. Using that `x` again in `main` would be an error:

```rust
#[derive(Debug)]
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

Results in:

```
error[E0382]: use of moved value: `x`
  --> foo.rs:11:17
   |
10 |     uses_foobar(x);
   |                 - value moved here
11 |     uses_foobar(x);
   |                 ^ value used here after move
   |
   = note: move occurs because `x` has type `Foobar`, which does not implement the `Copy` trait

error: aborting due to previous error

For more information about this error, try `rustc --explain E0382`.
```

Side note on copy trait way below...

## Dropping

When a value goes out of scope, its `Drop` trait (like a typeclass) is
used, and then the memory is freed. We can demonstrate this by writing
a `Drop` implementation for `Foobar`:

__Challenge__ Guess what the output of the program below is before
seeing the output.

```rust
#[derive(Debug)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) {
        println!("Dropping a Foobar: {:?}", self);
    }
}

fn uses_foobar(foobar: Foobar) {
    println!("I consumed a Foobar: {:?}", foobar);
}

fn main() {
    let x = Foobar(1);
    println!("Before uses_foobar");
    uses_foobar(x);
    println!("After uses_foobar");
}
```

__Output__

```
Before uses_foobar
I consumed a Foobar: Foobar(1)
Dropping a Foobar: Foobar(1)
After uses_foobar
```

Notice that the value is dropped before `After uses_foobar`. This is
because the value was moved into `uses_foobar`, and when that function
exits, the `drop` is called.

__Exercise 1__ There's a function in the standard library,
`std::mem::drop`, which drops a value immediately. Implement it.

## Lexical scoping

Scoping in Rust is currently lexical, though there's a Non Lexical
Lifetimes (NLL) proposal being merged in. (There's a [nice explanation
on Stack
Overflow](https://stackoverflow.com/questions/50251487/what-are-non-lexical-lifetimes)
of what NLL does.) We can demonstrate the currently-lexical nature of
scoping:

```rust
#[derive(Debug)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) {
        println!("Dropping a Foobar: {:?}", self);
    }
}

fn main() {
    println!("Before x");
    let _x = Foobar(1);
    println!("After x");
    {
        println!("Before y");
        let _y = Foobar(2);
        println!("After y");
    }
    println!("End of main");
}
```

__Syntax note__ Use a leading underscore `_` for variables that are
not used.

The output from this program is:

```
Before x
After x
Before y
After y
Dropping a Foobar: Foobar(2)
End of main
Dropping a Foobar: Foobar(1)
```

__CHALLENGE__ Remove the seemingly-superfluous braces and run the
program. Extra points: guess what the output will be before looking at
the actual output.

## Borrows/references (immutable)

Sometimes you want to be able to share a reference to a value without
moving ownership. Easy enough:

```rust
#[derive(Debug)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) {
        println!("Dropping a Foobar: {:?}", self);
    }
}

fn uses_foobar(foobar: &Foobar) {
    println!("I consumed a Foobar: {:?}", foobar);
}

fn main() {
    let x = Foobar(1);
    println!("Before uses_foobar");
    uses_foobar(&x);
    uses_foobar(&x);
    println!("After uses_foobar");
}
```

Things to notice:

* `uses_foobar` now takes a value of type `&Foobar`, which is
  "immutable reference to a `Foobar`."
* Inside `uses_foobar`, we don't need to explicitly dereference the
  `foobar` value, this is done automatically by Rust.
* In `main`, we can now use `x` in two calls to `uses_foobar`
* In order to create a reference from a value, we use `&` in front of
  the variable.

__Challenge__ When do you think the `Dropping a Foobar:` message gets
printed?

Remember from the last lesson that there is special syntax for a
parameter called `self`. The signature of `drop` above looks quite
different from `uses_foobar`. When you see `&mut self`, you can think
of it as `self: &mut Self`. Now it looks more similar to
`uses_foobar`.

__Exercise 2__ We'd like to be able to use object syntax for
`uses_foobar` as well. Create a method `use_it` on the `Foobar` type
that prints the `I consumed` message. Hint: you'll need to do this
inside `impl Foobar { ... }`.

## Multiple live references

We can change our `main` function to allow two references to `x` to
live at the same time. This version also adds explicit types on the
local variables, instead of relying on type inference:

```rust
fn main() {
    let x: Foobar = Foobar(1);
    let y: &Foobar = &x;
    println!("Before uses_foobar");
    uses_foobar(&x);
    uses_foobar(y);
    println!("After uses_foobar");
}
```

This is allowed in Rust, because:

1. Multiple read-only references to a variable cannot result in any
   data races
2. The lifetime of the value outlives the references to it. In other
   words, in this case, `x` lives at least as long as `y`.

Let's see two ways to break this.

### Reference outlives value

Remember that `std::mem::drop` from before? Check this out:

```rust
fn main() {
    let x: Foobar = Foobar(1);
    let y: &Foobar = &x;
    println!("Before uses_foobar");
    uses_foobar(&x);
    std::mem::drop(x);
    uses_foobar(y);
    println!("After uses_foobar");
}
```

This results in the error message:

```
error[E0505]: cannot move out of `x` because it is borrowed
  --> foo.rs:19:20
   |
16 |     let y: &Foobar = &x;
   |                       - borrow of `x` occurs here
...
19 |     std::mem::drop(x);
   |                    ^ move out of `x` occurs here

error: aborting due to previous error

For more information about this error, try `rustc --explain E0505`.
```

### Mutable reference with other references

You can also take _mutable_ references to a value. In order to avoid
data races, Rust does not allow value to be referenced mutably and
accessed in any other way at the same time.

```rust
fn main() {
    let mut x: Foobar = Foobar(1);
    let y: &mut Foobar = &mut x;
    println!("Before uses_foobar");
    uses_foobar(&x); // will fail!
    uses_foobar(y);
    println!("After uses_foobar");
}
```

Notice how the type of `y` is now `&mut Foobar`. Like Haskell, Rust
tracks mutability at the type level. Yay!

## Challenge

Try to guess which lines in the code below will trigger a compilation
error:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn main() {
    let x = Foobar(1);

    foo(x);
    foo(x);

    let mut y = Foobar(2);

    bar(&y);
    bar(&y);

    let z = &mut y;
    bar(&y);
    baz(&mut y);
    baz(z);
}

// move
fn foo(_foobar: Foobar) {
}

// read only reference
fn bar(_foobar: &Foobar) {
}

// mutable reference
fn baz(_foobar: &mut Foobar) {
}
```

## Mutable reference vs mutable variable

Something I didn't explain above was the `mut` before `x` in:

```rust
fn main() {
    let mut x: Foobar = Foobar(1);
    let y: &mut Foobar = &mut x;
    println!("Before uses_foobar");
    uses_foobar(&x);
    uses_foobar(y);
    println!("After uses_foobar");
}
```

By default, variables are immutable, and therefore do not allow any
kind of mutation. You cannot take a mutable reference to an immutable
variable, and therefore `x` must be marked as mutable. Here's an
easier way to see this:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn main() {
    let mut x = Foobar(1);

    x.0 = 2; // changes the 0th value inside the product

    println!("{:?}", x);
}
```

If you remove the `mut`, this will fail.

### Moving into mutable

This bothered me, and I assume it will bother other Haskellers. As
just mentioned, the following code will not compile:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn main() {
    let x = Foobar(1);

    x.0 = 2; // changes the 0th value inside the product

    println!("{:?}", x);
}
```

Obviously you can't mutate `x`. But let's change this ever so
slightly:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn main() {
    let x = Foobar(1);
    foo(x);
}

fn foo(mut x: Foobar) {

    x.0 = 2; // changes the 0th value inside the product

    println!("{:?}", x);
}
```

Before learning Rust, I would have objected to this: `x` is immutable,
and therefore we shouldn't be allowed to pass it to a function that
needs a mutable `x`. However, this isn't how Rust views the world. The
mutability here is a feature of the variable, not the value
itself. When you move the `x` into `foo`, `main` no longer has access
to `x`, and doesn't care if it's mutated. Inside `foo`, we've
explicitly stated that `x` can be mutated, so we're cool.

This is fairly different from how Haskell looks at things.

## Copy trait

We touched on this topic last time with numeric types vs
`String`. Let's hit it a little harder. Will the following code
compile or not?

```rust
fn uses_i32(i: i32) {
    println!("I consumed an i32: {}", i);
}

fn main() {
    let x = 1;
    uses_i32(x);
    uses_i32(x);
}
```

It _shouldn't_ work, right? `x` is moved into `uses_i32`, and then
used again. However, it compiles just fine! What gives?

Rust has a special trait, `Copy`, which indicates that a type is so
cheap that it can automatically be passed-by-value. That's exactly
what happens with `i32`. You can explicitly do this with the `Clone`
trait if desired:

```rust
#[derive(Debug, Clone)]
struct Foobar(i32);

impl Drop for Foobar {
    fn drop(&mut self) {
        println!("Dropping: {:?}", self);
    }
}

fn uses_foobar(foobar: Foobar) {
    println!("I consumed a Foobar: {:?}", foobar);
}

fn main() {
    let x = Foobar(1);
    uses_foobar(x.clone());
    uses_foobar(x);
}
```

__Challenge__ Why don't we need to use `x.clone()` on the second
`uses_foobar`? What happens if we put it in anyway?

__Exercise 3__ Change the code below, without modifying the `main`
function at all, so that it compiles and runs successfully. Some
hints: `Debug` is a special trait that can be automatically derived,
and in order to have a `Copy` implementation you also need a `Clone`
implementation.

```rust
#[derive(Debug)]
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

## Lifetimes

The term that goes along most with ownership is _lifetimes_. Every
value needs to be owned, and its owned for a certainly lifetime until
it's dropped. So far, everything we've looked at has involved implicit
lifetimes. However, as code gets more sophisticated, we need to be
more explicit about these lifetimes. We'll cover that another time.

## Exercise 4

Add an implementation of the `double` function to get this code to
compile, run, and output the number 2:

```rust
#[derive(Debug)]
struct Foobar(i32);

fn main() {
    let x: Foobar = Foobar(1);
    let y: Foobar = double(x);
    println!("{}", y.0);
}
```

Remember: to provide a return value from a function, put `->
ReturnType` after the parameter list.

## Notes on structs and enums

I mentioned above that `struct Foobar(i32)` is a newtype around an
`i32`. That's actually a special case of a more general _positional_
struct, where you can have 0 or more fields, named by their numeric
position. And positions start numbering at 0, as god and Linus
Torvalds intended.

There are some more examples:

```rust
struct NoFields; // may seem strange, we might cover examples of this later
struct OneField(i32);
struct TwoFields(i32, char);
```

You can also use record syntax:

```rust
struct Person {
    name: String,
    age: u32,
}
```

`struct`s are known as _product types_, which means they contain
multiple values. Rust also provides `enum`s, which are sum types, or
tagged unions. These are _alternatives_, where you select one of the
options. A simple enum would be:

```rust
enum Color {
    Red,
    Blue,
    Green,
}
```

But enums variants can also take values:

```rust
enum Shape {
    Square(u32), // size of one side
    Rectangle(u32, u32), // width and height
    Circle(u32), // radius
}
```

## Bouncy

Enough talk, let's fight! I want to create a simulation of a bouncing
ball. It's easier to demonstrate than explain:

<script id="asciicast-dB8y60T2G2Pp5GWNmxF8lF6tc" src="https://asciinema.org/a/dB8y60T2G2Pp5GWNmxF8lF6tc.js" async></script>

Let's step through the process of creating such a game together. I'll
provide the complete `src/main.rs` at the end of the lesson, but
strongly recommend you implement this together with me throughout the
sections below. Try to **avoid copy pasting**, but instead type in the
code yourself to get more comfortable with Rust syntax.

### Initialize the project

This part's easy:

```
$ cargo new bouncy
```

If you `cd` into that directory and run `cargo run`, you'll get output
like this:

```
$ cargo run
   Compiling bouncy v0.1.0 (/Users/michael/Desktop/bouncy)
    Finished dev [unoptimized + debuginfo] target(s) in 1.37s
     Running `target/debug/bouncy`
Hello, world!
```

The only file we're going to touch today is `src/main.rs`, which will
have the source code for our program.

### Define data structures

To track the ball bouncing around our screen, we need to know the
following information:

* The width of the box containing the ball
* The height of the box containing the ball
* The x and y coordinates of the ball
* The vertical direction of the ball (up or down)
* The horizontal direction of the ball (left or right)

We're going to define new datatypes for tracking the vertical and
horizontal direction, and use `u32`s for tracking the
position.

We can define `VertDir` as an `enum`. This is a simplified version of
what enums can handle, since we aren't given it any payload. We'll do
more sophisticated stuff later.

```rust
enum VertDir {
    Up,
    Down,
}
```

Go ahead and define a `HorizDir` as well that tracks whether we're
moving left or right. Now, to track a ball, we need to know its `x`
and `y` positions and its vertical and horizontal directions. This
will be a struct, since we're tracking multiple values instead of
(like an enum) choosing between different options.

```rust
struct Ball {
    x: u32,
    y: u32,
    vert_dir: VertDir,
    horiz_dir: HorizDir,
}
```

Define a `Frame` struct that tracks the width and height of the play
area. Then tie it all together with a `Game` struct:

```rust
struct Game {
    frame: Frame,
    ball: Ball,
}
```

### Create a new game

We can define a method on the `Game` type itself to create a new
game. We'll assign some default width and height and initial ball
position.

```rust
impl Game {
    fn new() -> Game {
        let frame = Frame {
            width: 60,
            height: 30,
        };
        let ball = Ball {
            x: 2,
            y: 4,
            vert_dir: VertDir::Up,
            horiz_dir: HorizDir::Left,
        };
        Game {frame, ball}
    }
}
```

__Challenge__ Rewrite this implementation to not use any `let`
statements.

Notice how we use `VertDir::Up`; the `Up` constructor is not imported
into the current namespace by default. Also, we can define `Game` with
`frame, ball` instead of `frame: frame, ball: ball` since the local
variable names are the same as the field names.

### Bounce

Let's implement the logic of a ball to bounce off of a wall. Let's
write out the logic:

* If the `x` value is 0, we're at the left of the frame, and therefore
  we should move right.
* If `y` is 0, move down.
* If `x` is one less than the width of the frame, we should move left.
* If `y` is one less than the height of the frame, we should move up.
* Otherwise, we should keep moving in the same direction.

We'll want to _modify_ the ball, and take the frame as a parameter. We'll implement this as a method on the `Ball` type.

```rust
impl Ball {
    fn bounce(&mut self, frame: &Frame) {
        if self.x == 0 {
            self.horiz_dir = HorizDir::Right;
        } else if self.x == frame.width - 1 {
            self.horiz_dir = HorizDir::Left;
        }

        ...
```

Go ahead and implement the rest of this function.

### Move

Once we know which direction to move in by calling `bounce`, we can
move the ball one position. We'll add this as another method within
`impl Ball`:

```rust
fn mv(&mut self) {
    match self.horiz_dir {
        HorizDir::Left => self.x -= 1,
        HorizDir::Right => self.x += 1,
    }
    ...
}
```

Implement the vertical half of this as well.

### Step

We need to add a method to `Game` to perform a step of the game. This
will involve both bouncing and moving. This goes inside `impl Game`:

```rust
fn step(&self) {
    self.ball.bounce(self.frame);
    self.ball.mv();
}
```

There are a few bugs in that implementation which you'll need to fix.

### Render

We need to be able to display the full state of the game. We'll see
that this initial implementation has its flaws, but we're going to do
this by printing the entire grid. We'll add a border, use the letter
`o` to represent the ball, and put spaces for all of the other areas
inside the frame. We'll use the `Display` trait for this.

Let's pull some of the types into our namespace. At the top of our
source file, add:

```rust
use std::fmt::{Display, Formatter};
```

Now, let's make sure we got the type signature correct:

```rust
impl Display for Game {
    fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
        unimplemented!()
    }
}
```

We can use the `unimplemented!()` macro to stub out our function
before we implement it. Finally, let's fill in a dummy `main` function
that will print the initial game:

```rust
fn main () {
    println!("{}", Game::new());
}
```

If everything is set up correctly, running `cargo run` will result in
a "not yet implemented" panic. If you get a compilation error, go fix
it now.

### Top border

Alright, now we can implement `fmt`. First, let's just draw the top
border. This will be a plus sign, a series of dashes (based on the
width of the frame), another plus sign, and a newline. We'll
use the `write!` macro, range syntax (`low..high`), and a `for` loop:

```rust
write!(fmt, "+");
for _ in 0..self.frame.width {
    write!(fmt, "-");
}
write!(fmt, "+\n");
```

Looks nice, but we get a compilation error:

```
error[E0308]: mismatched types
  --> src/main.rs:79:60
   |
79 |       fn fmt(&self, fmt: &mut Formatter) -> std::fmt::Result {
   |  ____________________________________________________________^
80 | |         write!(fmt, "+");
81 | |         for _ in 0..self.frame.width {
82 | |             write!(fmt, "-");
83 | |         }
84 | |         write!(fmt, "+\n");
   | |                           - help: consider removing this semicolon
85 | |     }
   | |_____^ expected enum `std::result::Result`, found ()
   |
   = note: expected type `std::result::Result<(), std::fmt::Error>`
              found type `()`
```

It says "considering removing this semicolon." Remember that putting
the semicolon forces our statement to evaluate to the unit value `()`,
but we want a `Result` value. And it seems like the `write!` macro is
giving us a `Result` value. Sure enough, if we drop the trailing
semicolon, we get something that works:

```
    Finished dev [unoptimized + debuginfo] target(s) in 0.55s
     Running `target/debug/bouncy`
+------------------------------------------------------------+
```

You may ask: what about all of the other `Result` values from the
other calls to `write!`? Good question! We'll get to that a bit later.

### Bottom border

The top and bottom border are exactly the same. Instead of duplicating
the code, let's define a closure that we can call twice. We introduce
a closure in Rust with the syntax `|args| { body }`. This closure will
take no arguments, and so will look like this:

```rust
let top_bottom = || {
    write!(fmt, "+");
    for _ in 0..self.frame.width {
        write!(fmt, "-");
    }
    write!(fmt, "+\n");
};

top_bottom();
top_bottom();
```

First we're going to get an error about `Result` and `()`
again. You'll need to remove two semicolons to fix this. Do that
now. Once you're done with that, you'll get a brand new error
message. Yay!

```
error[E0596]: cannot borrow `top_bottom` as mutable, as it is not declared as mutable
  --> src/main.rs:88:9
   |
80 |         let top_bottom = || {
   |             ---------- help: consider changing this to be mutable: `mut top_bottom`
...
88 |         top_bottom();
   |         ^^^^^^^^^^ cannot borrow as mutable
```

The error message tells us exactly what to do: stick a `mut` in the
middle of `let top_bottom`. Do that, and make sure it fixes
things. Now the question: why? The `top_bottom` closure has captured
the `fmt` variable from the environment. In order to use that, we need
to call the `write!` macro, which mutates that `fmt`
variable. Therefore, each call to `top_bottom` is itself a
mutation. Therefore, we need to mark `top_bottom` as mutable.

There are three different types of closure traits: `Fn`, `FnOnce`, and
`FnMut`. We'll get into the differences among these in a later
tutorial.

Anyway, we should now have both a top and bottom border in our output.

### Rows

Let's print each of the rows. In between the two `top_bottom()` calls,
we'll stick a `for` loop:

```rust
for row in 0..self.frame.height {
}
```

Inside that loop, we'll want to add the left border and the right
border:

```rust
write!(fmt, "|");
// more code will go here
write!(fmt, "|\n");
```

Go ahead and call `cargo run`, you're in for an unpleasant surprise:

```
error[E0501]: cannot borrow `*fmt` as mutable because previous closure requires unique access
  --> src/main.rs:91:20
   |
80 |         let mut top_bottom = || {
   |                              -- closure construction occurs here
81 |             write!(fmt, "+");
   |                    --- first borrow occurs due to use of `fmt` in closure
...
91 |             write!(fmt, "|");
   |                    ^^^ borrow occurs here
...
96 |         top_bottom()
   |         ---------- first borrow used here, in later iteration of loop
```

Oh no, we're going to have to deal with the borrow checker!

### Fighting the borrow checker

Alright, remember before that the `top_bottom` closure capture a
mutable reference to `fmt`? Well that's causing us some trouble
now. There can only be one mutable reference in play at a time, and
`top_bottom` is holding it for the entire body of our method. Here's a
simple workaround in this case: take `fmt` as a parameter to the
closure, instead of capturing it:

```rust
let top_bottom = |fmt: &mut Formatter| {
```

Go ahead and fix the calls to `top_bottom`, and you should get output
that looks like this (some extra rows removed).

```
+------------------------------------------------------------+
||
||
||
||
...
+------------------------------------------------------------+
```

Alright, now we can get back to...

### Columns

Remember that `// more code will go here` comment? Time to replace it!
We're going to use another `for` loop for each column:

```rust
for column in 0..self.frame.width {
    write!(fmt, " ");
}
```

Running `cargo run` will give you a complete frame, nice!
Unfortunately, it doesn't include our ball. We want to write a `o`
character instead of space when `column` is the same as the ball's
`x`, and the same thing for `y`. Here's a partial implementation:

```rust
let c = if row == self.ball.y {
    'o'
} else {
    ' '
};
write!(fmt, "{}", c);
```

There's something wrong with the output (test with `cargo run`). Fix
it and your render function will be complete!

### The infinite loop

We're almost done! We need to add an infinite loop in our `main`
function that:

* Prints the game
* Steps the game
* Sleeps for a bit of time

We'll target 30 FPS, so we want to sleep for 33ms. But how do we sleep
in Rust? To figure that out, let's go to [the Rust standard library
docs](https://doc.rust-lang.org/std/index.html) and [search for
`sleep`](https://doc.rust-lang.org/std/index.html?search=sleep). The
first result is
[`std::thread::sleep`](https://doc.rust-lang.org/std/thread/fn.sleep.html),
which seems like a good bet. Check out the docs there, especially the
wonderful example, to understand this code.

```rust
fn main () {
    let game = Game::new();
    let sleep_duration = std::time::Duration::from_millis(33);
    loop {
        println!("{}", game);
        game.step();
        std::thread::sleep(sleep_duration);
    }
}
```

There's one compile error in this code. Try to anticipate what it
is. If you can't figure it out, ask the compiler, then fix it. You
should get a successful `cargo run` that shows you a bouncing ball.

### Problems

There are two problems I care about in this implementation:

* The output can be a bit jittery, especially on a slow terminal. We
  should really be using something like the `curses` library to handle
  double buffering of the output.
* If you ran `cargo run` before, you probably didn't see it. Run
  `cargo clean` and `cargo build` to force a rebuild, and you should
  see the following warning:

```
warning: unused `std::result::Result` which must be used
  --> src/main.rs:88:9
   |
88 |         top_bottom(fmt);
   |         ^^^^^^^^^^^^^^^^
   |
   = note: this `Result` may be an `Err` variant, which should be handled
```

I mentioned this problem above: we're ignoring failures coming from
the calls to the `write!` macro in most cases but throwing away the
`Result` using a semicolon. There's a nice, single character solution
to this problem. This forms the basis of proper error handling in
Rust. However, we'll save that for another time. For now, we'll just
ignore the warning.

### Complete source

You can find the complete source code for this implementation [as a
Github
gist](https://gist.github.com/snoyberg/5307d493750d7b48c1c5281961bc31d0). Reminder:
it's much better if you step through the code above and implement it
yourself.

I've added one piece of syntax we haven't covered yet in that tutorial, at the
end of the call to `top_bottom`. We'll cover that in much more detail next
week.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
