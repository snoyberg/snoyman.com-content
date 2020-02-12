Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Kick the Tires."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

The following code is broken:

```rust
fn main() {
    let val: String = String::from("Hello, World!");
    printer(val);
    printer(val);
}

fn printer(val: String) {
    println!("The value is: {}", val);
}
```

We got an error message about a _move_. We'll learn more about moves
in the next lesson, when we discuss ownership. There are two basic
solutions. First, the less-than-ideal one:

### Clone

We've moved the original `val` into the first call to `printer`, and
can't use it again. One workaround is to instead move a _clone_ of
`val` into that call, leaving the original unaffected:

```rust
fn main() {
    let val: String = String::from("Hello, World!");
    printer(val.clone());
    printer(val);
}

fn printer(val: String) {
    println!("The value is: {}", val);
}
```

Notice that I only cloned `val` the first time, not the second
time. We don't need `val` again after the second call, so it's safe to
move it. Using an extra clone is expensive, since it requires
allocating memory and performing a buffer copy.

Speaking of it being expensive...

### Pass by reference

Instead of moving the value, we can instead pass it into the `printer`
function by reference. Let's first try to achieve that by just
modifying `printer`:

```rust
fn main() {
    let val: String = String::from("Hello, World!");
    printer(val);
    printer(val);
}

fn printer(val: &String) {
    println!("The value is: {}", val);
}
```

This doesn't work, because when we call `printer`, we're still giving
it a `String` and not a reference to a `String`. Fixing that is pretty
easy:

```rust
fn main() {
    let val: String = String::from("Hello, World!");
    printer(&val);
    printer(&val);
}
```

Note that the ampersand means both:

* A reference to this type, and
* Take a reference to this value

There's an even better way to write `printer`:

```rust
fn printer(val: &str) {
    println!("The value is: {}", val);
}
```

By using `str` instead of `String`, we can pass in string literals,
and do not need to force the allocation of a heap object. We'll get to
this in more detail when we discuss strings.

## Exercise 2

This is the broken code:

```rust
fn main() {
    let i = 1;

    loop {
        println!("i == {}", i);
        if i >= 10 {
            break;
        } else {
            i += 1;
        }
    }
}
```

The error message we get from the compiler is pretty informative:

> cannot assign twice to immutable variable `i`

In order to fix this, we change the variable from immutable to mutable:

```rust
fn main() {
    let mut i = 1;
    ...
```

## Exercise 3

This exercise asked you do see when you could leave out
semicolons. Here's a simple rule:

* The statement is the last statement in a block
* The type of the expression is unit

For example, in this code, removing the semicolon is fine:

```rust
fn main() {
    for i in 1..11 {
        println!("i == {}", i);
    }
}
```

That said, it tends to be somewhat idiomatic to leave semicolons on
expressions like these which are purely effectful.

## Exercise 4

This exercise was to implement FizzBuzz in Rust. Repeating the rules here:

* Print the numbers 1 to 100
* If the number is a multiple of 3, output fizz instead of the number
* If the number is a multiple of 5, output buzz instead of the number
* If the number is a multiple of 3 __and__ 5, output fizzbuzz instead of the number

Here's one possible solution using `if`/`else` fallbacks:

```rust
fn main() {
    for i in 1..101 {
        if i % 3 == 0 && i % 5 == 0 {
            println!("fizzbuzz");
        } else if i % 3 == 0 {
            println!("fizz");
        } else if i % 5 == 0 {
            println!("buzz");
        } else {
            println!("{}", i);
        }
    }
}
```

This has at least one downside: it will need to test `i % 3 == 0` and `i % 5 == 0` potentially multiple times. Under the surface, the compiler may optimize this away. But still, the repeated modulus calls are just sitting in the code taunting us! We can instead use _pattern matching_:

```rust
fn main() {
    for i in 1..101 {
        match (i % 3 == 0, i % 5 == 0) {
            (true, true) => println!("fizzbuzz"),
            (true, false) => println!("fizz"),
            (false, true) => println!("buzz"),
            (false, false) => println!("{}", i),
        }
    }
}
```

Or, if you want to have some fun with wildcard matching:

```rust
fn main() {
    for i in 1..101 {
        match (i % 3, i % 5) {
            (0, 0) => println!("fizzbuzz"),
            (0, _) => println!("fizz"),
            (_, 0) => println!("buzz"),
            (_, _) => println!("{}", i),
        }
    }
}
```

I'm not going to tell you which of these is the "best" solution. And
there are certainly other implementations that could be
attempted. This was meant to give you a feel for some more Rust
constructs.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
