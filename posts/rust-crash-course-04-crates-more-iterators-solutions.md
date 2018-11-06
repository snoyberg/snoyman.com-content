Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Crates, I/O, and more iterators."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

You can find my complete solution [as a Github
Gist](https://gist.github.com/snoyberg/936ecefd7b6fabc2438e51f02bfe36cb). If
your solution looks a bit different than mine, don't worry. Also, see
if there's anything interesting you can learn from my implementation,
or some improvements you'd like to make to it.

## Exercise 2

```rust
struct TheAnswer;

impl Iterator for TheAnswer {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        Some(42)
    }
}
```

## Exercise 3

Let's start with the simpler solution:

```rust
struct Fibs {
    x: u32,
    y: u32,
}

fn fibs() -> Fibs {
    Fibs {
        x: 0,
        y: 1,
    }
}

impl Iterator for Fibs {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        let orig_x = self.x;
        let orig_y = self.y;

        self.x = orig_y;
        self.y = orig_x + orig_y;

        Some(orig_x)
    }
}

fn main() {
    for i in fibs().take(10) {
        println!("{}", i);
    }
}
```

However, if you bump that `take(10)` to `take(47)`, the end of your
output will look like:

```
701408733
1134903170
thread 'main' panicked at 'attempt to add with overflow', foo.rs:21:18
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

One solution would be to bump to a `u64`, but that's just delaying the
problem. Instead, we can use Rust's checked addition method:

```rust
fn next(&mut self) -> Option<u32> {
    let orig_x = self.x;
    let orig_y = self.y;

    match orig_x.checked_add(orig_y) {
        // overflow
        None => None,

        // no overflow
        Some(new_y) => {
            self.x = orig_y;
            self.y = new_y;

            Some(orig_x)
        }
    }
}
```

Now our stream will stop as soon as overflow occurs.

If you want to get _really_ advanced here, you could actually output
two more values. To do so, we need to assign to a *derefenced* value
and use an `enum` to track our state:

```rust
fn next(&mut self) -> Option<u32> {
    use Fibs::*;
    match *self {
        Done => None,
        OneLeft(x) => {
            *self = Done;
            Some(x)
        }
        Running(orig_x, orig_y) => {
            *self = match orig_x.checked_add(orig_y) {
                // overflow
                None => OneLeft(orig_y),
                Some(new_y) => Running(orig_y, new_y),
            };

            Some(orig_x)
        }
    }
}
```

## Exercise 4

```rust
impl<I> Iterator for Doubler<I>
    where
    I: Iterator,
    I::Item: std::ops::Add<Output=I::Item> + Copy,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(x) => Some(x + x),
        }
    }
}
```

## Exercise 5

The `fold` method takes two parameters: the initial value, and a
function for adding the running total with the next value. One
approach using closures is:

```rust
fn main() {
    let res = (1..11).fold(0, |x, y| x + y);
    println!("{}", res);
}
```

Another approach is to directly refer to the addition
function. Remember how there was a `Mul` trait for the `*` operator?
There's also an `Add` trait for addition:

```rust
fn main() {
    let res = (1..11).fold(0, std::ops::Add::add);
    println!("{}", res);
}
```

As for writing our own `sum` function: we'll end up back in the
situation where things are generic and we have to provide appropriate
traits. We'll follow a similar approach with using `From` and a `u8`:

```rust
fn sum<I>(iter: I) -> I::Item
    where
    I: Iterator,
    I::Item: std::ops::Add<Output=I::Item> + From<u8>,
{
    iter.fold(From::from(0u8), std::ops::Add::add)
}
```

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
