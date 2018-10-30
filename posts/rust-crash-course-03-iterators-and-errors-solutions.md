Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Iterators and Errors."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

The trick here is to "wrap" the `Args` data type with the `Skip` data
type:

```rust
use std::env::{args, Args};
use std::iter::Skip;

fn main() {
    let args: Skip<Args> = args().skip(1);
    for arg in args {
        println!("{}", arg);
    }
}
```

You may have noticed that we didn't need to mark `args` as
mutable. That's because we are moving the `args` value into the `for`
loop, meaning that any mutation to it by the `for` loop cannot be seen
in the `main` function.

## Exercise 2

When we call `parse` in the context of that example, type inference
tells us that the `width` and `height` results must be `u32`s, since
they are used as fields in `Frame`. Rust is able to determine what
implementation to use based on the needed return type. Cool!

Yet again, Haskellers are rolling their eyes and saying "old news."

## Exercise 3

Complete source file:

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
        ParseArgs(std::env::args())
    }


    fn require_arg(&mut self) -> Result<String, ParseError> {
        match self.0.next() {
            None => Err(ParseError::TooFewArgs),
            Some(s) => Ok(s),
        }
    }

    fn require_no_args(&mut self) -> Result<(), ParseError> {
        match self.0.next() {
            Some(_) => Err(ParseError::TooManyArgs),
            None => Ok(()),
        }
    }
}

fn parse_u32(s: String) -> Result<u32, ParseError> {
    match s.parse() {
        Err(_) => Err(ParseError::InvalidInteger(s)),
        Ok(x) => Ok(x),
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

## Exercise 4

We want to ensure a minimum size for the width and height. First,
let's add two more variants to the `ParseError` enum:

```rust
WidthTooSmall(u32),
HeightTooSmall(u32),
```

Then add the following to the `parse_args` function, just before the `Ok`:

```rust
if width < 2 {
    return Err(WidthTooSmall(width));
}
if height < 2 {
    return Err(HeightTooSmall(height));
}
```

## Exercise 5

This is another perfect time to pull out our trailing question mark!

```rust
fn main () -> Result<(), self::parse_args::ParseError> {
    let frame = parse_args::parse_args()?;
    let mut game = Game::new(frame);
    let sleep_duration = std::time::Duration::from_millis(33);
    loop {
        println!("{}", game);
        game.step();
        std::thread::sleep(sleep_duration);
    }
}
```

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
