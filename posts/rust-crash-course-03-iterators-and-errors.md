Last time, we finished off with a [bouncy ball
implementation](https://gist.github.com/snoyberg/5307d493750d7b48c1c5281961bc31d0)
with some downsides: lackluster error handling and ugly buffering. It
also had another limitation: a static frame size. Today, we're going
to address all of these problems, starting with that last one: let's
get some command line arguments to control the frame size.

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

Like last time, I'm going to expect you, the reader, to be making
changes to the source code along with me. Make sure to _actually type
in the code while reading_!

## Command line arguments

We're going to modify our application as follows:

* Accept two command line arguments: the width and the height
* Both must be valid `u32`s
* Too many or too few command line arguments will result in an error message

Sounds easy enough. In a real application, we would use a proper
argument-handling library, like
[clap](https://crates.io/crates/clap). But for now, we're going lower
level. Like we did for the sleep function, let's start by [searching
the standard library
docs](https://doc.rust-lang.org/std/index.html?search=args) for the
word `args`. The first two entries both look relevant.

* `std::env::Args` An iterator over the arguments of a process, yielding a `String` value for each argument.
* `std::env::args` Returns the arguments which this program was started with (normally passed via the command line).

Now's a good time to mention that, by strong convention:

* Module names (like `std` and `env`) and function names (like `args`) are `snake_cased`
* Types (like `Args`) are `PascalCased`
    * Exception: primitives like `u32` and `str` are lower case

The `std` module has an `env` module. The `env` module has both an
`Args` type and a `args` function. Why do we need both? Even more
strangely, let's look at the type signature for the `args` function:

```rust
pub fn args() -> Args
```

The `args` _function_ returns a value of type `Args`. If `Args` was a
type synonym for, say, a vector of `String`s, this would make
sense. But that's not the case. And if you [check out its
docs](https://doc.rust-lang.org/std/env/struct.Args.html), there
aren't any fields or methods exposed on `Args`, only trait
implementations!

## The extra datatype pattern

Maybe there's a proper term for this in Rust, but I haven't seen it
myself yet. (If someone has, please let me know so I can use the
proper term.) There's a pervasive pattern in the Rust ecosystem, which
in my experience starts with iterators and continues to more advanced
topics like futures and async I/O.

* We want to have composable interfaces
* We also want high performance
* Therefore, we define lots of helper data types that allow the
  compiler to perform some great optimizations
* And we define traits as an interface to let these types compose
  nicely with each other

Sound abstract? Don't worry, we'll make that concrete in a bit. Here's
the practical outcome of all of this:

* We end up programming quite a bit against traits, which provide a
  common abstractions and lots of helper functions
* We get a matching data type for many common functions
* Often times, our type signatures will end up being massive,
  representing all of the different composition we performed (though
  the new-ish `-> impl Iterator` style helps with that significantly,
  see [the announcement blog post](https://blog.rust-lang.org/2018/05/10/Rust-1.26.html#impl-trait)
  for more details)

Alright, with that out of the way, let's get back to command line
arguments!

## CLI args via iterators

Let's play around in an empty file before coming back to bouncy. (Either use `cargo new` and `cargo run`, or use `rustc` directly, your call.) If I click on the expand button next to the `Iterator` trait on the `Args` docs page, I see this function:

```rust
fn next(&mut self) -> Option<String>
```

Let's play with that a bit:

```rust
use std::env::args;

fn main() {
    let mut args = args(); // Yes, that name shadowing works
    println!("{:?}", args.next());
    println!("{:?}", args.next());
    println!("{:?}", args.next());
    println!("{:?}", args.next());
}
```

Notice that we had to use `let mut`, since the `next` method will
mutate the value. Now I'm going to run this with `cargo run foo bar`:

```
$ cargo run foo bar
   Compiling args v0.1.0 (/Users/michael/Desktop/tmp/args)
    Finished dev [unoptimized + debuginfo] target(s) in 1.60s
     Running `target/debug/args foo bar`
Some("target/debug/args")
Some("foo")
Some("bar")
None
```

Nice! It gives us the name of our executable, followed by the command
line arguments, returning `None` when there's nothing left. (For
pedants out there: command line arguments aren't technically
_required_ to have the command name as the first argument, it's just a
really strong convention most tools follow.)

Let's play with this some more. Can you write a loop that prints out
all of the command line arguments and then exits? Take a minute, and
then I'll provide some answers.

Alright, done? Cool, let's see some examples! First, we'll `loop` with
`return`.

```rust
use std::env::args;

fn main() {
    let mut args = args();
    loop {
        match args.next() {
            None => return,
            Some(arg) => println!("{}", arg),
        }
    }
}
```

We also don't need to use `return` here. Instead of returning from the
function, we can just `break` out of the loop:

```rust
use std::env::args;

fn main() {
    let mut args = args();
    loop {
        match args.next() {
            None => break,
            Some(arg) => println!("{}", arg),
        }
    }
}
```

Or, if you want to save on some indentation, you can use the `if let`.

```rust
use std::env::args;

fn main() {
    let mut args = args();
    loop {
        if let Some(arg) = args.next() {
            println!("{}", arg);
        } else {
            break;
            // return would work too, but break is nicer
            // here, as it is more narrowly scoped
        }
    }
}
```

You can _also_ use `while let`. Try to guess what that would look like
before checking the next example:

```rust
use std::env::args;

fn main() {
    let mut args = args();
    while let Some(arg) = args.next() {
        println!("{}", arg);
    }
}
```

Getting better! Alright, one final example:

```rust
use std::env::args;

fn main() {
    for arg in args() {
        println!("{}", arg);
    }
}
```

Whoa, what?!? Welcome to one of my favorite aspects of Rust. Iterators
are a concept built into the language directly, via `for` loops. A
`for` loop will automate the calling of `next()`. It also hides away
the fact that there's some mutable state at play, at least to some
extent. This is a powerful concept, and allows a lot of code to end up
with a more functional style, something I happen to be a big fan of.

## Skipping

It's all well and good that the first arguments in the name of the
executable. But we typically don't care about that. Can we somehow
skip that in our output? Well, here's one approach:

```rust
use std::env::args;

fn main() {
    let mut args = args();
    let _ = args.next(); // drop it on the floor
    for arg in args {
        println!("{}", arg);
    }
}
```

That works, but it's a bit clumsy, especially compared to our previous
version that had no mutable variables. Maybe there's some other way to
skip things. Let's [search the standard library
again](https://doc.rust-lang.org/std/env/struct.Args.html?search=skip). I
see the first results as
[`std::iter::Skip`](https://doc.rust-lang.org/std/iter/struct.Skip.html)
and
[`std::iter::Iterator::skip`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.skip). The
former is a data type, and the latter is a method on the `Iterator`
trait. Since our `Args` type implements the `Iterator` trait, we can
use it. Nice!

__Side note__ Haskellers: `skip` is like `drop` in most Haskell
libraries, like `Data.List` or `vector`. `drop` has a totally
different meaning in Rust (dropping owned data), so `skip` is a better
name in Rust.

Let's look at some signatures from the docs above:

```rust
pub struct Skip<I> { /* fields omitted */ }
fn skip(self, n: usize) -> Skip<Self>
```

Hmm... deep breaths. `Skip` is a data type that is _parameterized_
over some data type, `I`. This is a common pattern in iterators:
`Skip` wraps around an existing data type and adds some new
functionality to how it iterates. The `skip` method will _consume_ an
existing iterator, take the number of arguments to skip, and return a
new `Skip<OrigDataType>` value. How do I know it consumes the original
iterator? The first parameter is `self`, not `&self` or `&mut self`.

That seemed like a lot of concepts. Fortunately, _usage_ is pretty
easy:

```rust
use std::env::args;

fn main() {
    for arg in args().skip(1) {
        println!("{}", arg);
    }
}
```

Nice!

__Exercise 1__ Type inference lets the program above work just fine
without any type annotations. However, it's a good idea to get used to
the generated types, since you'll see them [all too
often](https://twitter.com/snoyberg/status/1054393245122334720) in
error messages. Get the program below to compile by fixing the type
signature. Try to do it without using compiler at first, since the
error messages will almost give the answer away.

```rust
use std::env::{args, Args};
use std::iter::Skip;

fn main() {
    let args: Args = args().skip(1);
    for arg in args {
        println!("{}", arg);
    }
}
```

This layering-of-datatypes approach, as mentioned above, is a real
boon to performance. Iterator-heavy code will often compile down to an
efficient loop, comparable with the best hand-rolled loop you could
have written. However, iterator code is much higher level, more
declarative, and easy to maintain and extend.

There's a lot more to iterators, but we're going to stop there for the
moment, since we still want to process our command line parameters,
and we need to learn one more thing first.

## Parsing integers

If you search the standard library for `parse`, you'll find the
[`str::parse`
method](https://doc.rust-lang.org/std/primitive.str.html#method.parse). The
documentation does a good job of explaining things, I won't repeat
that here. Please go read that now.

OK, you're back? Turbofish is a funny name, right?

Take a crack at writing a program that prints the result of parsing
each command line argument as a `u32`, then check my version:

```rust
fn main() {
    for arg in std::env::args().skip(1) {
        println!("{:?}", arg.parse::<u32>());
    }
}
```

And let's try running it:

```
$ cargo run one 2 three four 5 6 7
Err(ParseIntError { kind: InvalidDigit })
Ok(2)
Err(ParseIntError { kind: InvalidDigit })
Err(ParseIntError { kind: InvalidDigit })
Ok(5)
Ok(6)
Ok(7)
```

When the parse is successful, we get the `Ok` variant of the `Result`
enum. When the parse fails, we get the `Err` variant, with a
[`ParseIntError`](https://doc.rust-lang.org/std/num/struct.ParseIntError.html)
telling us what went wrong. (The type signature on `parse` itself uses
some associated types to indicate this type, we're not going to get
into that right now.)

This is a common pattern in Rust. Rust has no runtime exceptions, so
we track potential failure at the type level with actual values.

__Side note__ You may think of `panic`s as similar to runtime
exceptions, and to some extent they are. However, you're not able to
properly recover from `panic`s, making them different in practice from
how runtime exceptions are used in other languages like Python.

## Parse our command line

We're finally ready to get started on our actual command line parsing!
We're going to be overly tedious in our implementation, especially
with our data types. After we finish implementing this in a blank
file, we'll move the code into the bouncy implementation
itself. First, let's define a data type to hold a successful parse,
which will contain the width and the height.

__Challenge__ Will this be a struct or an enum? Can you try
implementing this yourself first?

Since we want to hold onto multiple values, we'll be using a
`struct`. I'd like to use named fields, so we have:

```rust
struct Frame {
    width: u32,
    height: u32,
}
```

Next, let's define an error type to represent all of the things that
can go wrong during this parse. We have:

* Too few arguments
* Too many arguments
* Invalid integer

__Challenge__ Are we going to use a struct or an enum this time?

This time, we'll use an enum, because we'll only detect one of these
problems (whichever we notice first). Officianados of web forms and
applicative parsing may scoff at this and say we should detect _all_
errors, but we're going to be lazy.

```rust
enum ParseError {
    TooFewArgs,
    TooManyArgs,
    InvalidInteger(String),
}
```

Notice that the `InvalidInteger` variant takes a payload, the `String`
it failed parsing. This is what makes `enum`s in Rust so much more
powerful than enumerations in most other languages.

__Challenge__ We're going to write a `parse_args` helper function. Can
you guess what its type signature will be?

Combining all of the knowledge we established above, here's an
implementation:

```rust
#[derive(Debug)]
struct Frame {
    width: u32,
    height: u32,
}

#[derive(Debug)]
enum ParseError {
    TooFewArgs,
    TooManyArgs,
    InvalidInteger(String),
}

fn parse_args() -> Result<Frame, ParseError> {
    use self::ParseError::*; // bring variants into our namespace

    let mut args = std::env::args().skip(1);

    match args.next() {
        None => Err(TooFewArgs),
        Some(width_str) => {
            match args.next() {
                None => Err(TooFewArgs),
                Some(height_str) => {
                    match args.next() {
                        Some(_) => Err(TooManyArgs),
                        None => {
                            match width_str.parse() {
                                Err(_) => Err(InvalidInteger(width_str)),
                                Ok(width) => {
                                    match height_str.parse() {
                                        Err(_) => Err(InvalidInteger(height_str)),
                                        Ok(height) => Ok(Frame {
                                            width,
                                            height,
                                        }),
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn main() {
    println!("{:?}", parse_args());
}
```

Holy nested blocks Batman, that is a _lot_ of indentation! The pattern
is pretty straightforward:

* Pattern match
* If we got something bad, stop with an `Err`
* If we got something good, keep going

Haskellers at this point are screaming about `do` notation and
monads. Ignore them. We're in the land of Rust, we don't take
**kind**ly to those things around here. (Someone please yell at me for
that terrible pun.)

__Exercise 2__ Why didn't we need to use the turbofish on the call to
`parse` above?

What we want to do is _return_ early from our function. You know what
keyword can help with that? That's right: `return`!

```rust
fn parse_args() -> Result<Frame, ParseError> {
    use self::ParseError::*;

    let mut args = std::env::args().skip(1);

    let width_str = match args.next() {
        None => return Err(TooFewArgs),
        Some(width_str) => width_str,
    };

    let height_str = match args.next() {
        None => return Err(TooFewArgs),
        Some(height_str) => height_str,
    };

    match args.next() {
        Some(_) => return Err(TooManyArgs),
        None => (),
    }

    let width = match width_str.parse() {
        Err(_) => return Err(InvalidInteger(width_str)),
        Ok(width) => width,
    };

    let height = match height_str.parse() {
        Err(_) => return Err(InvalidInteger(height_str)),
        Ok(height) => height,
    };

    Ok(Frame {
        width,
        height,
    })
}
```

Much nicer to look at! However, it's still a bit repetitive, and
littering those `return`s everywhere is subjectively not very nice. In
fact, while typing this up, I accidentally left off a few of the
`return`s and got to stare at some long error messages. (Try that for
yourself.)

## Question mark

__Side note__ The trailing question mark we're about to introduce used
to be the `try!` macro in Rust. If you're confused about the seeming
overlap: it's simply a transition to new syntax.

The pattern above is so common that Rust has built in syntax for
it. If you put a question mark after an expression, it basically does
the whole match/return-on-Err thing for you. It's more powerful than
we'll demonstrate right now, but we'll get to that extra power a bit
later.

To start off, we're going to define some helper functions to:

* Require another argument
* Require that there are no more arguments
* Parse a `u32`

All of these need to return `Result` values, and we'll use a
`ParseError` for the error case in all of them. The first two
functions need to take a mutable reference to our arguments. (As a
side note, I'm going to stop using the `skip` method now, because if I
do it will give away the solution to exercise 1.)

```rust
use std::env::Args;

fn require_arg(args: &mut Args) -> Result<String, ParseError> {
    match args.next() {
        None => Err(ParseError::TooFewArgs),
        Some(s) => Ok(s),
    }
}

fn require_no_args(args: &mut Args) -> Result<(), ParseError> {
    match args.next() {
        Some(_) => Err(ParseError::TooManyArgs),
        // I think this looks a little weird myself.
        // But we're wrapping up the unit value ()
        // with the Ok variant. You get used to it
        // after a while, I guess
        None => Ok(()),
    }
}

fn parse_u32(s: String) -> Result<u32, ParseError> {
    match s.parse() {
        Err(_) => Err(ParseError::InvalidInteger(s)),
        Ok(x) => Ok(x),
    }
}
```

Now that we have these helpers defined, our `parse_args` function is
much easier to look at:

```rust
fn parse_args() -> Result<Frame, ParseError> {
    let mut args = std::env::args();

    // skip the command name
    let _command_name = require_arg(&mut args)?;

    let width_str = require_arg(&mut args)?;
    let height_str = require_arg(&mut args)?;
    require_no_args(&mut args)?;
    let width = parse_u32(width_str)?;
    let height = parse_u32(height_str)?;

    Ok(Frame { width, height })
}
```

Beautiful!

## Forgotten question marks

What do you think happens if you forget the question mark on the `let
width_str` line? If you do so:

* `width_str` will contain a `Result<String, ParseError>` instead of a `String`
* The call to `parse_u32` will not type check

```
error[E0308]: mismatched types
  --> src/main.rs:50:27
   |
50 |     let width = parse_u32(width_str)?;
   |                           ^^^^^^^^^ expected struct `std::string::String`, found enum `std::result::Result`
   |
   = note: expected type `std::string::String`
              found type `std::result::Result<std::string::String, ParseError>`
```

That's nice. But what will happen if we forget the question mark on
the `require_no_args` call? We never use the output value there, so it
will type check just fine. Now we have the age old problem of C: we're
accidentally ignoring error codes!

Well, not so fast. Check out this wonderful warning from the compiler:

```
warning: unused `std::result::Result` which must be used
  --> src/main.rs:49:5
   |
49 |     require_no_args(&mut args);
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: #[warn(unused_must_use)] on by default
   = note: this `Result` may be an `Err` variant, which should be handled
```

That's right: Rust will detect if you've ignored a potential
failure. There _is_ a hole in this in the current code sample:

```rust
let _command_name = require_arg(&mut args);
```

That doesn't trigger the warning, since in `let _name = blah;`, the
leading underscore says "I know what I'm doing, I don't care about
this value." Instead, it's better to write the code without the `let`:

```rust
require_arg(&mut args);
```

Now we get a warning, which can be solved with the added trailing
question mark.

## Exercise 3

It would be more convenient to use method call syntax. Let's define a
helper data type to make this possible. Fill in the implementation of
the code below.

```rust
#[derive(Debug)]
struct Frame {
    width: u32,
    height: u32,
}

#[derive(Debug)]
enum ParseError {
    TooFewArgs,
    TooManyArgs,
    InvalidInteger(String),
}

struct ParseArgs(std::env::Args);

impl ParseArgs {
    fn new() -> ParseArgs {
        unimplemented!()
    }


    fn require_arg(&mut self) -> Result<String, ParseError> {
        match self.0.next() {
        }
    }

    fn require_no_args(&mut self) -> Result<(), ParseError> {
        unimplemented!()
    }
}

fn parse_args() -> Result<Frame, ParseError> {
    let mut args = ParseArgs::new();

    // skip the command name
    args.require_arg()?;

    let width_str = args.require_arg()?;
    let height_str = args.require_arg()?;
    args.require_no_args()?;
    let width = parse_u32(width_str)?;
    let height = parse_u32(height_str)?;

    Ok(Frame { width, height })
}

fn main() {
    println!("{:?}", parse_args());
}
```

## Updating bouncy

This next bit should be done as a Cargo project, not with
`rustc`. Let's start a new empty project:

```
$ cargo new bouncy-args --bin
$ cd bouncy-args
```

Next, let's get the [old
code](https://gist.githubusercontent.com/snoyberg/5307d493750d7b48c1c5281961bc31d0/raw/8f467e87f69a197095bda096cbbb71d8d813b1d7/main.rs)
and place it in `src/main.rs`. You can copy-paste manually, or run:

```
$ curl https://gist.githubusercontent.com/snoyberg/5307d493750d7b48c1c5281961bc31d0/raw/8f467e87f69a197095bda096cbbb71d8d813b1d7/main.rs > src/main.rs
```

Run `cargo run` and make sure it works. You can use `Ctrl-C` to kill
the program.

We already wrote fully usable argument parsing code above. Instead of
putting it in the same source file, let's put it in its own file. In
order to do so, we're going to have to play with modules in Rust.

For convenience, you can [view the full source code](https://gist.github.com/snoyberg/568899dc3ae6c82e54809efe283e4473) as a Gist. We need to put this in `src/parse_args.rs`:

```
$ curl https://gist.githubusercontent.com/snoyberg/568899dc3ae6c82e54809efe283e4473/raw/2ee261684f81745b21e571360b1c5f5d77b78fce/parse_args.rs > src/parse_args.rs
```

If you run `cargo build` now, it won't even look at
`parse_args.rs`. Don't believe me? Add some invalid content to the top
of that file and run `cargo build` again. Nothing happens, right? We
need to tell the compiler that we've got another module in our
project. We do that by modifying `src/main.rs`. Add the following line
to the top of your file:

```rust
mod parse_args;
```

If you put in that invalid line before, running `cargo build` should
now result in an error message. Perfect! Go ahead and get rid of that
invalid line and make sure everything compiles and runs. We won't be
accepting command line arguments yet, but we're getting closer.

## Use it!

We're currently getting some dead code warnings, since we aren't using
anything from the new module:

```
warning: struct is never constructed: `Frame`
 --> src/parse_args.rs:2:1
  |
2 | struct Frame {
  | ^^^^^^^^^^^^
  |
  = note: #[warn(dead_code)] on by default

warning: enum is never used: `ParseError`
 --> src/parse_args.rs:8:1
  |
8 | enum ParseError {
  | ^^^^^^^^^^^^^^^

warning: function is never used: `parse_args`
  --> src/parse_args.rs:14:1
   |
14 | fn parse_args() -> Result<Frame, ParseError> {
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Let's fix that. To start off, add the following to the top of your
`main` function, just to prove that we can, in fact, use our new module:

```rust
println!("{:?}", parse_args::parse_args());
return; // don't start the game, our output will disappear
```

Also, add a `pub` in front of the items we want to access from the
`main.rs` file, namely:

* `struct Frame`
* `enum ParseError`
* `fn parse_args`

Running this gets us:

```
$ cargo run
   Compiling bouncy-args v0.1.0 (/Users/michael/Desktop/tmp/bouncy-args)
warning: unreachable statement
   --> src/main.rs:115:5
    |
115 |     let mut game = Game::new();
    |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    |
    = note: #[warn(unreachable_code)] on by default

warning: variable does not need to be mutable
   --> src/main.rs:115:9
    |
115 |     let mut game = Game::new();
    |         ----^^^^
    |         |
    |         help: remove this `mut`
    |
    = note: #[warn(unused_mut)] on by default

    Finished dev [unoptimized + debuginfo] target(s) in 0.67s
     Running `target/debug/bouncy-args`
Err(TooFewArgs)
```

It's nice that we get an unreachable statement warning. It's also a
bit weird that `game` is no longer required to be
mutable. Strange. But most importantly: our argument parsing is
working!

Let's try to use this. We'll modify the `Game::new()` method to accept
a `Frame` as input:

```rust
impl Game {
    fn new(frame: Frame) -> Game {
        let ball = Ball {
            x: 2,
            y: 4,
            vert_dir: VertDir::Up,
            horiz_dir: HorizDir::Left,
        };
        Game {frame, ball}
    }

    ...
}
```

And now we can rewrite our `main` function as:

```rust
fn main () {
    match parse_args::parse_args() {
        Err(e) => {
            // prints to stderr instead of stdout
            eprintln!("Error parsing args: {:?}", e);
        },
        Ok(frame) => {
            let mut game = Game::new(frame);
            let sleep_duration = std::time::Duration::from_millis(33);
            loop {
                println!("{}", game);
                game.step();
                std::thread::sleep(sleep_duration);
            }
        }
    }
}
```

## Mismatched types

We're good, right? Not quite:

```
error[E0308]: mismatched types
   --> src/main.rs:114:38
    |
114 |             let mut game = Game::new(frame);
    |                                      ^^^^^ expected struct `Frame`, found struct `parse_args::Frame`
    |
    = note: expected type `Frame`
               found type `parse_args::Frame`
```

We now have two different definitions of `Frame`: in our `parse_args`
module, and in `main.rs`. Let's fix that. First, delete the `Frame`
declaration in `main.rs`. Then add the following after our `mod
parse_args;` statement:

```rust
use self::parse_args::Frame;
```

`self` says we're finding a module that's a child of the current
module.

## Public and private

Now everything will work, right? Wrong again! `cargo build` will vomit
a bunch of these errors:

```
error[E0616]: field `height` of struct `parse_args::Frame` is private
  --> src/main.rs:85:23
   |
85 |         for row in 0..self.frame.height {
   |
```

By default, identifiers are private in Rust. In order to expose them
from one module to another, you need to add the `pub` keyword. For
example:

```rust
pub width: u32,
```

Go ahead and add `pub` as needed. Finally, if you run `cargo run`, you
should see `Error parsing args: TooFewArgs`. And if you run `cargo run
5 5`, you should see a much smaller frame than before. Hurrah!

## Exercise 4

What happens if you run `cargo run 0 0`? How about `cargo run 1 1`?
Put in some better error handling in `parse_args`.

## Exit code

Alright, one final irritation. Let's provide some invalid arguments
and inspect the exit code of the process:

```
$ cargo run 5
Error parsing args: TooFewArgs
$ echo $?
0
```

For those not familiar: a 0 exit code means everything went OK. That's
clearly not the case here! If we search the standard library, it seems
the
[`std::process::exit`](https://doc.rust-lang.org/std/process/fn.exit.html)
can be used to address this. Go ahead and try using that to solve the
problem here.

However, we've got one more option: we can return a `Result` straight
from `main`!

```rust
fn main () -> Result<(), self::parse_args::ParseError> {
    match parse_args::parse_args() {
        Err(e) => {
            return Err(e);
        },
        Ok(frame) => {
            let mut game = Game::new(frame);
            let sleep_duration = std::time::Duration::from_millis(33);
            loop {
                println!("{}", game);
                game.step();
                std::thread::sleep(sleep_duration);
            }
        }
    }
}
```

__Exercise 5__ Can you do something to clean up the nesting a bit here?

## Better error handling

The error handling problem we had in the last lesson involved the call
to `top_bottom`. I've already included a solution to that in the
download of the code provided. Guess what I changed since last time
and then check the code to confirm that you're right.

If you're following _very_ closely, you may be surprised that there
aren't more warnings about unused `Result` values coming from other
calls to `write!`. As far as I can tell, this is in fact a [bug in the
Rust compiler](https://github.com/rust-lang/rust/issues/55240).

Still, it would be good practice to fix up those calls to
`write!`. Take a stab at doing so.

## Next time

We still didn't fix our double buffering problem, we'll get to that
next time. We're also going to introduce some more error handling from
the standard library. And maybe we'll get to play a bit more with
iterators as well.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
