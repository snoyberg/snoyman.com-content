Below are the solutions to the exercises from the last Rust Crash
Course lesson, "Lifetimes and slices."

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Exercise 1

If you try just throwing in the `ref` keyword like this:

```rust
match person.age {
    Some(ref age) => {
        println!("Age is {}", age);
        *age += 1;
    }
    None => println!("No age provided"),
}
```

You'll get an error message:

```
error[E0594]: cannot assign to immutable borrowed content `*age`
  --> src/main.rs:16:13
   |
14 |         Some(ref age) => {
   |              ------- help: use a mutable reference instead: `ref mut age`
15 |             println!("Age is {}", age);
16 |             *age += 1;
   |             ^^^^^^^^^ cannot borrow as mutable
```

Instead, you need to say `ref mut age`. And if you're like me and
regularly type in `mut ref age` instead of `ref mut age`, don't worry,
the compiler's got your back:

```
error: the order of `mut` and `ref` is incorrect
  --> src/main.rs:14:14
   |
14 |         Some(mut ref age) => {
   |              ^^^^^^^ help: try switching the order: `ref mut`

error: aborting due to previous error
```

## Exercise 2

You need to provide mutable references for the two arguments to
`swap`. Additionally, in order to get a mutable reference to `res`,
`res` itself needs to be `mut`able.

```rust
fn next(&mut self) -> Option<T> {
    let mut res = None;
    std::mem::swap(&mut res, &mut self.next);
    res
}
```

## Exercise 3

We need to have two different parameters, and ensure that `ret` and
the return value have the same lifetime parameter:

```rust
fn message_and_return<'a, 'b>(msg: &'a String, ret: &'b String) -> &'b String {
    println!("Printing the message: {}", msg);
    ret
}
```

## Exercise 4

Since the data is stored in the program executable itself, it lives
for the entire program execution. Therefore, the lifetime is
`'static`:

```rust
fn main() {
    let bytearray1: &'static [u8; 22] = b"Hello World in binary!";
    let bytearray2: &'static [u8] = b"Hello World in binary!";
    println!("{:?}", bytearray1);
    println!("{:?}", bytearray2);
}
```

## Exercise 5

This is a great use case for the iterator method `count`:

```rust
fn main() {
    for arg in std::env::args() {
        println!(
            "arg: {}, characters: {}, bytes: {}",
            arg,
            arg.chars().count(),
            arg.bytes().count(),
            );
    }
}
```

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
