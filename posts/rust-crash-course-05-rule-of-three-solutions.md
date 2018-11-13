Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Rule of Three - Parameters, Iterators, and Closures."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

```rust
fn double(x: &mut u32) {
    *x *= 2;
}

fn main() {
    let mut x = 5;
    double(&mut x);
    println!("{}", x);
}
```

Notice that the variable `x` does not need to be mutable, since we're
only modifying the value it references.

## Exercise 2

The (IMO) straightforward solution is:

```rust
struct InfiniteUnit;

impl IntoIterator for InfiniteUnit {
    type Item = ();
    type IntoIter = InfiniteUnitIter;

    fn into_iter(self) -> Self::IntoIter {
        InfiniteUnitIter
    }
}

struct InfiniteUnitIter;

impl Iterator for InfiniteUnitIter {
    type Item = ();
    fn next(&mut self) -> Option<()> {
        Some(())
    }
}

fn main() {
    let mut count = 0;
    for _ in InfiniteUnit {
        count += 1;
        println!("count == {}", count);
        if count >= 5 {
            break;
        }
    }
}
```

However, if you want to be a bit more clever, there's already a
function in the standard library that creates an infinite iterator,
called
[`repeat`](https://doc.rust-lang.org/std/iter/fn.repeat.html). Using
that, you can bypass the extra struct here:

```rust
struct InfiniteUnit;

impl IntoIterator for InfiniteUnit {
    type Item = ();
    type IntoIter = std::iter::Repeat<()>;

    fn into_iter(self) -> Self::IntoIter {
        std::iter::repeat(())
    }
}

fn main() {
    let mut count = 0;
    for _ in InfiniteUnit {
        count += 1;
        println!("count == {}", count);
        if count >= 5 {
            break;
        }
    }
}
```

## Exercise 3

The closure version:

```rust
fn main() {
    let msg: &str = "Hi!";
    let say_hi = |msg| println!("{}", msg);
    say_hi(msg);
    say_hi(msg);
}
```

And the function version:

```rust
fn main() {
    let msg: &str = "Hi!";
    fn say_hi(msg: &str) {
        println!("{}", msg);
    }
    say_hi(msg);
    say_hi(msg);
}
```

Since `say_hi` is no longer referring to any variables in the local
scope, it doesn't need to be a closure.

## Exercise 4

```rust
fn main() {
    call_with_hi(say_message);
    call_with_hi(say_message);
}

fn say_message(msg: &str) {
    println!("{}", msg);
}

fn call_with_hi<F>(f: F)
    where F: Fn(&str)
{
    f("Hi!");
}
```

## Exercise 5

The first error message we get is:

```
error[E0599]: no method named `map` found for type `std::vec::Vec<u32>` in the current scope
 --> main.rs:5:23
  |
5 |         for i in nums.map(unimplemented!()) {
  |                       ^^^
  |
  = note: the method `map` exists but the following trait bounds were not satisfied:
          `&mut std::vec::Vec<u32> : std::iter::Iterator`
          `&mut [u32] : std::iter::Iterator`
```

Looks like we need to get an `Iterator` out of our `nums`. We have
three different choices: `into_iter()`, `iter()`, and
`iter_mut()`. Since we need to use the result multiple times, and
don't need any mutation, `iter()` seems like the right call. Once we
replace `nums.map` with `nums.iter().map`, we can move on to the
`unimplemented!()` bit.

We need a closure that will double a number. That's pretty easy: `|x|
x * 2`. Plugging that in works! Extra challenge: is that closure a
`FnOnce`, `FnMut`, or `Fn`?

## Exercise 6

You need to add a `.unwrap()` call on the `create` call:

```rust
use std::io::Write;
let mut file = std::fs::File::create("mylog.txt").unwrap();
file.write_all(b"I was clicked.\n");
```

Like this, you'll get a warning from the compiler that you've ignored
the `Result` coming from `write_all`. That's bad practice, and the
compiler is rightfully yelling at you. You can fix that with
`unwrap()`. However, that's also bad practice :).

## Exercise 7

```rust
extern crate gtk;

use gtk::prelude::*;

use gtk::{Button, Window, WindowType};

use std::cell::RefCell;
use std::io::Write;

fn main() -> Result<(), Box<std::error::Error>> {
    gtk::init()?;

    let window = Window::new(WindowType::Toplevel);
    window.set_title("First GTK+ Program");
    window.set_default_size(350, 70);
    let button = Button::new_with_label("Click me!");
    window.add(&button);
    window.show_all();

    window.connect_delete_event(|_, _| {
        gtk::main_quit();
        Inhibit(false)
    });

    let file = std::fs::File::create("mylog.txt")?;
    let file = RefCell::new(file);
    button.connect_clicked(move |_| {
        match file.borrow_mut().write_all(b"I was clicked.\n") {
            Ok(()) => (),
            Err(e) => eprintln!("Error writing to file: {}", e),
        }
    });

    gtk::main();

    Ok(())
}
```

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
