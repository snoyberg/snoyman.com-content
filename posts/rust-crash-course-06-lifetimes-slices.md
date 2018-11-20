We've glossed over some details of lifetimes and sequences of values so far.
It's time to dive in and learn about lifetimes and _slices_ correctly.

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Printing a person

Let's look at some fairly unsurprising code:

```rust
#[derive(Debug)]
struct Person {
    name: Option<String>,
    age: Option<u32>,
}

fn print_person(person: Person) {
    match person.name {
        Some(name) => println!("Name is {}", name),
        None => println!("No name provided"),
    }

    match person.age {
        Some(age) => println!("Age is {}", age),
        None => println!("No age provided"),
    }
}

fn main() {
    print_person(Person {
        name: Some(String::from("Alice")),
        age: Some(30),
    });
}
```

Fairly simple, and a nice demonstration of pattern matching. However,
let's throw in one extra line. Try adding this at the beginning of the
`print_person` function:

```rust
println!("Full Person value: {:?}", person);
```

All good. We're printing the full contents of the `Person` and then
pattern matching. But try adding that line to the _end_ of the
function, and you'll get a compilation error:

```
error[E0382]: use of partially moved value: `person`
  --> main.rs:18:41
   |
9  |         Some(name) => println!("Name is {}", name),
   |              ---- value moved here
...
18 |     println!("Full Person value: {:?}", person);
   |                                         ^^^^^^ value used here after move
   |
   = note: move occurs because the value has type `std::string::String`, which does not implement the `Copy` trait

error: aborting due to previous error
```

__NOTE__ This is an error with the Rust compiler I'm using,
1.30.1. However, there are plans in place to improve this situation.

The problem is that we've consumed a part of the `person` value, and
therefore cannot display it. We can fix that by setting it
again. Let's make the `person` argument `mut`able, and then fill
in the moved `person.name` with a default `None` value:

```rust
fn print_person(mut person: Person) {
    match person.name {
        Some(name) => println!("Name is {}", name),
        None => println!("No name provided"),
    }

    match person.age {
        Some(age) => println!("Age is {}", age),
        None => println!("No age provided"),
    }

    person.name = None;

    println!("Full Person value: {:?}", person);
}
```

That compiles, but now the output is confusingly:

```
Name is Alice
Age is 30
Full Person value: Person { name: None, age: Some(30) }
```

Notice how the `name` in the last line in `None`, when ideally it should be
`Some(Alice)`. We can do better, by returning the name from the `match`:

```rust
person.name = match person.name {
    Some(name) => {
        println!("Name is {}", name);
        Some(name)
    },
    None => {
        println!("No name provided");
        None
    }
};
```

But that's decidely inelegant. Let's take a step back. Do we actually
need to consume/move the `person.name` at all? Not really. It should
work to do everything by reference. So let's go back and avoid the
move entirely, by using a borrow:

```rust
match &person.name {
    Some(name) => println!("Name is {}", name),
    None => println!("No name provided"),
}
```

Much better! We don't need to put the borrow on `person.age` though,
since the `u32` is `Copy`able. Here, we're pattern matching on a
reference, and therefore the `name` is _also_ a reference.

However, we can be more explicit about that with the `ref`
keyword. This keyword says that, when pattern matching, we want the
pattern to be a reference, _not_ a move of the original value. ([More
info in the Rust
book.](https://doc.rust-lang.org/book/second-edition/ch18-03-pattern-syntax.html#creating-references-in-patterns-with-ref-and-ref-mut))
We end up with:

```rust
match person.name {
    Some(ref name) => println!("Name is {}", name),
    None => println!("No name provided"),
}
```

In our case, this is the same basic result as `&person.name`.

## Birthday!

Let's modify our code so that, when printing the age, we also increase
the age by 1. First stab is below. Note that the code won't compile,
try to predict why:

```rust
match person.age {
    Some(age) => {
        println!("Age is {}", age);
        age += 1;
    }
    None => println!("No age provided"),
}
```

We're trying to mutate the local `age` binding, but it's
immutable. Well, that's easy enough to fix, just replace `Some(age)`
with `Some(mut age)`. That compiles, but with a warning:

```
warning: value assigned to `age` is never read
  --> src/main.rs:16:13
   |
16 |             age += 1;
   |             ^^^
   |
   = note: #[warn(unused_assignments)] on by default
```

And then the output is:

```
Name is Alice
Age is 30
Full Person value: Person { name: Some("Alice"), age: Some(30) }
```

Notice how on the last line, the age is still 30, not 31. Take a
minute and try to understand what's happening here... Done? Cool.

1. We pattern match on `person.age`
2. If it's `Some`, we need to move the age into the local `age`
   binding
3. But since the type is `u32`, it will make a copy and move the copy
4. When we increment the age, we're incrementing a copy, which is never used.

We can try solving this by taking a mutable reference to `person.age`:

```rust
fn print_person(person: Person) {
    match person.name {
        Some(ref name) => println!("Name is {}", name),
        None => println!("No name provided"),
    }

    match &mut person.age {
        Some(age) => {
            println!("Age is {}", age);
            age += 1;
        }
        None => println!("No age provided"),
    }

    println!("Full Person value: {:?}", person);
}
```

The compiler complains: `age` is a `&mut u32`, but we're trying to use
`+=` on it:

```
error[E0368]: binary assignment operation `+=` cannot be applied to type `&mut u32`
  --> src/main.rs:16:13
   |
16 |             age += 1;
   |             ---^^^^^
   |             |
   |             cannot use `+=` on type `&mut u32`
   |
   = help: `+=` can be used on 'u32', you can dereference `age`: `*age`
```

The compiler taketh, and the compiler giveth as well: we just need to
dereference the `age` reference. Close, but one more error:

```
error[E0596]: cannot borrow field `person.age` of immutable binding as mutable
  --> src/main.rs:13:16
   |
7  | fn print_person(person: Person) {
   |                 ------ consider changing this to `mut person`
...
13 |     match &mut person.age {
   |                ^^^^^^^^^^ cannot mutably borrow field of immutable binding

error: aborting due to previous error
```

Again, the compiler tells us exactly how to solve the problem: make
`person` `mut`able. Go ahead and make that change, and everything
should work.

__Exercise 1__ In the case of `person.name`, we came up with two
solutions: borrow the `person.name`, or use the `ref` keyword. The
same two styles of solutions will work for our current problem. We've
just demonstrated the borrow approach. Try to solve this instead using
the `ref` keyword.

## The single iterator

Let's make a silly little iterator which produces a single
value. We'll track whether or not the value has been produced by using
an `Option`:

```rust
struct Single<T> {
    next: Option<T>,
}
```

Let's make a helper function to create `Single` values:

```rust
fn single<T>(t: T) -> Single<T> {
    Single {
        next: Some(t),
    }
}
```

And let's write a `main` that tests that this works as expected:

```rust
fn main() {
    let actual: Vec<u32> = single(42).collect();
    assert_eq!(vec![42], actual);
}
```

If you try to compile that, you'll get an error message:

```
error[E0599]: no method named `collect` found for type `Single<{integer}>` in the current scope
  --> src/main.rs:12:39
   |
1  | struct Single<T> {
   | ---------------- method `collect` not found for this
...
12 |     let actual: Vec<u32> = single(42).collect();
   |                                       ^^^^^^^
   |
   = note: the method `collect` exists but the following trait bounds were not satisfied:
           `&mut Single<{integer}> : std::iter::Iterator`
   = help: items from traits can only be used if the trait is implemented and in scope
   = note: the following trait defines an item `collect`, perhaps you need to implement it:
           candidate #1: `std::iter::Iterator`
```

We need to provide an `Iterator` implementation in order to use
`collect()`. The `Item` is going to be `T`. And we've already got a
great `Option<T>` available for the return value from the `next`
function:

```rust
impl<T> Iterator for Single<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        self.next
    }
}
```

Unfortunately this doesn't work:

```
error[E0507]: cannot move out of borrowed content
  --> src/main.rs:21:9
   |
21 |         self.next
   |         ^^^^ cannot move out of borrowed content

error: aborting due to previous error
```

Oh, right. We can't move the result value out, since our `next`
function only mutable borrows `self`. Let's try some pattern matching:

```rust
match self.next {
    Some(next) => Some(next),
    None => None,
}
```

Except this _also_ involves moving out of a borrow, so it fails. Let's
try one more time: we'll move into a local variable, set `self.next`
to `None` (so it doesn't repeat the value again), and return the local
variable:

```rust
fn next(&mut self) -> Option<T> {
    let res = self.next;
    self.next = None;
    res
}
```

Nope, the compiler is _still_ not happy! I guess we'll just have to
give up on our grand vision of a `Single` iterator. We could of course
just cheat:

```rust
fn next(&mut self) -> Option<T> {
    None
}
```

But while that compiles, it fails our test at runtime:

```
thread 'main' panicked at 'assertion failed: `(left == right)`
  left: `[42]`,
 right: `[]`', src/main.rs:13:5
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

## Swap

What we did above was attempt to swap the `self.next` with a local
variable. However, the borrow checker wasn't a fan of the approach we
took. However, there's a helper function in the standard library,
`std::mem::swap`, which may be able to help us. It looks like:

```rust
pub fn swap<T>(x: &mut T, y: &mut T)
```

And sure enough, we can use it to solve our problem:

```rust
fn next(&mut self) -> Option<T> {
    let res = None;
    std::mem::swap(res, self.next);
    res
}
```

__Exercise 2__ The code above doesn't quite compile, though the
compiler can guide you to a correct solution. Try to identify the
problems above and fix them yourself. Failing that, ask the compiler
to help you out.

## replace and take

Did you find that whole create-a-temp-variable thing a bit verbose?
Yeah, it does to the authors of the Rust standard library too. There's
a helper function that bypasses that temporary variable:

```rust
fn next(&mut self) -> Option<T> {
    std::mem::replace(&mut self.next, None)
}
```

Much nicer! However, that __still__ seems like more work for something
that should be really easy. And fortunately, yet again, it does to the
authors of the Rust standard library too. This pattern of replacing
the value in an `Option` with `None` and then working with the
original value is common enough that they've given it a name and a
method: `take`.

```rust
fn next(&mut self) -> Option<T> {
    self.next.take()
}
```

And we're done!

## Lifetimes

We've briefly mentioned lifetimes in previous lessons, but it's time
to get a bit more serious about them. Let's look at a simple usage of
references:

```rust
struct Person {
    name: String,
    age: u32,
}

fn get_name(person: &Person) -> String {
    person.name
}

fn main() {
    let alice = Person {
        name: String::from("Alice"),
        age: 30,
    };
    let name = get_name(&alice);
    println!("Name: {}", name);
}
```

This code doesn't compile. Our `get_name` function takes a reference
to a `Person`, and then tries to move that person's `name` in its
result. This isn't possible. One solution would be to clone the name:

```rust
fn get_name(person: &Person) -> String {
    person.name.clone()
}
```

While this works, it's relatively inefficient. We like to avoid making
copies when we can. Instead, let's simply return a reference to the
name:

```rust
fn get_name(person: &Person) -> &String {
    &person.name
}
```

Hurrah! But let's make our function a little bit more complicated. We
now want a function that will take _two_ `Person`s, and return the
name of the older one. That sounds fairly easy to write:

```rust
struct Person {
    name: String,
    age: u32,
}

fn get_older_name(person1: &Person, person2: &Person) -> &String {
    if person1.age >= person2.age {
        &person1.name
    } else {
        &person2.name
    }
}

fn main() {
    let alice = Person {
        name: String::from("Alice"),
        age: 30,
    };
    let bob = Person {
        name: String::from("Bob"),
        age: 35,
    };
    let name = get_older_name(&alice, &bob);
    println!("Older person: {}", name);
}
```

Unfortunately, the compiler is quite cross with us:

```
error[E0106]: missing lifetime specifier
 --> src/main.rs:6:58
  |
6 | fn get_older_name(person1: &Person, person2: &Person) -> &String {
  |                                                          ^ expected lifetime parameter
  |
  = help: this function's return type contains a borrowed value, but the signature does not say whether it is borrowed from `person1` or `person2`
```

That error message is remarkably clear. Our function is returning a
borrowed value. That value must be borrowed from _somewhere_. The only
two options\* are `person1` and `person2`. And it seems that Rust
needs to know this for some reason.

\* This is a small fib, see "static lifetime" below.

Remember how we have some rules about references? References cannot
outlive the original values they come from. We need to track how long
the result value is allowed to live, which must be less than or equal
to the time the value it came from lives. This whole concept is
_lifetimes_.

For reasons we'll get to in a bit (under "lifetime elision"), we can
often bypass the need to explicitly talk about lifetimes. However,
sometimes we do need to be explicit. To do this, we introduce some new
parameters. But this time, they are _lifetime parameters_, which begin
with a single quote and are lower case. Usually, they are just a
single letter. For example:

```rust
fn get_older_name<'a, 'b>(person1: &'a Person, person2: &'b Person) -> &String
```

We still get an error from the compiler because our return value
doesn't have a lifetime. Should we choose `'a` or `'b`? Or maybe we
should create a new `'c` and try that? Let's start off with `'a`. We
get the error message:

```
error[E0623]: lifetime mismatch
  --> src/main.rs:10:9
   |
6  | fn get_older_name<'a, 'b>(person1: &'a Person, person2: &'b Person) -> &'a String {
   |                                                         ----------     ----------
   |                                                         |
   |                                                         this parameter and the return type are declared with different lifetimes...
...
10 |         &person2.name
   |         ^^^^^^^^^^^^^ ...but data from `person2` is returned here
```

That makes sense: since our result may come from `person2`, we have no
guarantee that the `'a` lifetime parameter is less than or equal to
the `'b` lifetime parameter. Fortunately, we can explicitly state
that, in the same way that we state that types implement some traits:

```rust
fn get_older_name<'a, 'b: 'a>(person1: &'a Person, person2: &'b Person) -> &'a String {
```

And this actually compiles! Alternatively, in this case, we can just
completely bypass the second lifetime parameter, and say that
`person1` and `person2` must have the same lifetime, which must be the
same as the return value:

```rust
fn get_older_name<'a>(person1: &'a Person, person2: &'a Person) -> &'a String {
```

If you're like me, you may think that this would be overly
limiting. For example, I initially thought that with the signature
above, this code wouldn't compile:

```rust
fn main() {
    let alice = Person {
        name: String::from("Alice"),
        age: 30,
    };
    foo(&alice);
}

fn foo(alice: &Person) {
    let bob = Person {
        name: String::from("Bob"),
        age: 35,
    };
    let name = get_older_name(&alice, &bob);
    println!("Older person: {}", name);
}
```

After all, the lifetime for `alice` is demonstrably bigger than the
lifetime for `bob`. However, the semantics for lifetimes in functions
signatures is that all of the values have at least the same
lifetime. If they happen to live a bit longer, no harm, no foul.

## Requirement for multiple lifetime parameters

So can we cook up an example where multiple lifetime parameters are absolutely necessary? Sure!

```rust
fn message_and_return(msg: &String, ret: &String) -> &String {
    println!("Printing the message: {}", msg);
    ret
}

fn main() {
    let name = String::from("Alice");
    let msg = String::from("This is the message");
    let ret = message_and_return(&msg, &name);
    println!("Return value: {}", ret);
}
```

This code won't compile, because we need some lifetime parameters. So
let's use our trick from above, and use the same parameter:

```rust
fn message_and_return<'a>(msg: &'a String, ret: &'a String) -> &'a String {
```

That compiles, but let's make our calling code a bit more complicated:

```rust
fn main() {
    let name = String::from("Alice");
    let ret = foo(&name);
    println!("Return value: {}", ret);
}

fn foo(name: &String) -> &String {
    let msg = String::from("This is the message");
    message_and_return(&msg, &name)
}
```

Now the compiler isn't happy:

```
error[E0597]: `msg` does not live long enough
  --> src/main.rs:14:25
   |
14 |     message_and_return(&msg, &name)
   |                         ^^^ borrowed value does not live long enough
15 | }
   | - borrowed value only lives until here
   |
note: borrowed value must be valid for the anonymous lifetime #1 defined on the function body at 12:1...
  --> src/main.rs:12:1
   |
12 | / fn foo(name: &String) -> &String {
13 | |     let msg = String::from("This is the message");
14 | |     message_and_return(&msg, &name)
15 | | }
   | |_^
```

We've stated that the return value must live the same amount of time
as the `msg` parameter. But we return the return value _outside_ of
the `foo` function, while the `msg` value will not live beyond the end
of `foo`.

The calling code should be fine, we just need to tell Rust that it's
OK if the `msg` parameter has a shorter lifetime than the return
value.

__Exercise 3__ Modify the signature of `message_and_return` so that
the code compiles and runs.

## Lifetime elision

Why do we sometimes get away with skipping the lifetimes, and
sometimes we need to include them? There are rules in the language
called "lifetime elision." Instead of trying to cover this myself,
I'll refer to the Nomicon:

https://doc.rust-lang.org/nomicon/lifetime-elision.html

## Static lifetime

Above, I implied that if you return a reference, then it must have the
same lifetime as one of its input parameters. This mostly makes sense,
because otherwise we'd have to conjure some arbitrary lifetime out of
thin air. However, it's also a lie. There's one special lifetime that
survives the entire program, called `'static`. And here's some fun
news: you've implicitly used it since the first Hello World we wrote
together!

Every single string literal is in fact a reference with the lifetime
of `'static`.

```rust
fn name() -> &'static str {
    "Alice"
}

fn main() {
    println!("{}", name());
}
```

## Arrays, slices, vectors, and String

Here's another place where we've been cheeky. What's the difference
between `String` and `str`? Both of these have popped up quite a
bit. We'll get to those in a little bit. First, we need to talk about
arrays, slices, and vectors.

### Arrays

To my knowledge, the best official documentation on arrays is in [the
API docs
themselves](https://doc.rust-lang.org/std/primitive.array.html). Arrays
are contiguous blocks of memory containing a single type of data with
a fixed length. The type is represented as `[T; N]`, where `T` is the
type of value, and `N` is the length of the array. And like any sane
programming language, arrays are 0-indexed in Rust.

There are two syntaxes for initiating arrays. List literal syntax
(like Javascript, Python, or Haskell):

```rust
fn main() {
    let nums: [u32; 5] = [1, 2, 3, 4, 5];
    println!("{:?}", nums);
}
```

And a repeat expression:

```rust
fn main() {
    let nums: [u32; 5] = [42; 5];
    println!("{:?}", nums);
}
```

You can make arrays mutable and then, well, mutate them:

```rust
fn main() {
    let mut nums: [u32; 5] = [42; 5];
    nums[2] += 1;
    println!("{:?}", nums);
}
```

That's very nice, but what if you need something more dynamic? For
that, we have...

### Vec

A `Vec` is a "contiguous, growable array type." You can `push` and
`pop`, check its length, and access via index in O(1) time. We also
have a nifty `vec!` macro for constructing them:

```rust
fn main() {
    let mut v: Vec<u32> = vec![1, 2, 3];
    v.push(4);
    assert_eq!(v.pop(), Some(4));
    v.push(4);
    v.push(5);
    v.push(6);
    assert_eq!(v.pop(), Some(6));
    assert_eq!(v[2], 3);
    println!("{:?}", v); // 1, 2, 3, 4, 5
}
```

### Slices

I'm going to write a helper function that prints all the values in a
`Vec`:

```rust
fn main() {
    let v: Vec<u32> = vec![1, 2, 3];
    print_vals(v);
}

fn print_vals(v: Vec<u32>) {
    for i in v {
        println!("{}", i);
    }
}
```

Of course, since this is a pass-by-value, the following doesn't compile:

```rust
fn main() {
    let mut v: Vec<u32> = vec![1, 2, 3];
    print_vals(v);
    v.push(4);
    print_vals(v);
}
```

Easy enough to fix: have `print_vals` take a reference to a `Vec`:

```rust
fn main() {
    let mut v: Vec<u32> = vec![1, 2, 3];
    print_vals(&v);
    v.push(4);
    print_vals(&v);
}

fn print_vals(v: &Vec<u32>) {
    for i in v {
        println!("{}", i);
    }
}
```

Unfortunately, this doesn't generalize to, say, an array:

```rust
fn main() {
    let a: [u32; 5] = [1, 2, 3, 4, 5];
    print_vals(&a);
}
```

This fails since `print_vals` takes a `&Vec<u32>`, but we've provided
a `&[u32; 5]`. But this is pretty disappointing. A dynamic vector and
a fixed length array behave the same for so many things. Wouldn't it
be nice if there was something that generalized both of these?

Enter slices. To quote the Rust book:

> Slices let you reference a contiguous sequence of elements in a
> collection rather than the whole collection.

To make this all work, we need to change the signature of `print_vals`
to:

```rust
fn print_vals(v: &[u32]) {
```

`&[u32]` is a reference to a _slice_ of `u32`s. A slice can be created
from an array _or_ a `Vec`, not to mention some other cases as
well. (We'll discuss how the `&` borrow operator works its magic in a
bit.) As a general piece of advice, if you're receiving a parameter
which is a sequence of values, try to use a slice, as it will give the
caller much more control about where the data comes from.

I played a bit of a word game above, switching between "reference to a
slice" and "a slice." Obviously we're using a reference. Can we
dereference a slice reference and get the slice itself? Let's try!

```rust
fn print_vals(vref: &[u32]) {
    let v = *vref;
    for i in v {
        println!("{}", i);
    }
}
```

The compiler is cross with us again:

```
error[E0277]: the size for values of type `[u32]` cannot be known at compilation time
 --> src/main.rs:7:9
  |
7 |     let v = *vref;
  |         ^ doesn't have a size known at compile-time
  |
  = help: the trait `std::marker::Sized` is not implemented for `[u32]`
  = note: to learn more, visit <https://doc.rust-lang.org/book/second-edition/ch19-04-advanced-types.html#dynamically-sized-types-and-sized>
  = note: all local variables must have a statically known size

error[E0277]: the size for values of type `[u32]` cannot be known at compilation time
 --> src/main.rs:8:14
  |
8 |     for i in v {
  |              ^ doesn't have a size known at compile-time
  |
  = help: the trait `std::marker::Sized` is not implemented for `[u32]`
  = note: to learn more, visit <https://doc.rust-lang.org/book/second-edition/ch19-04-advanced-types.html#dynamically-sized-types-and-sized>
  = note: required by `std::iter::IntoIterator::into_iter`

error[E0277]: the trait bound `[u32]: std::iter::Iterator` is not satisfied
 --> src/main.rs:8:14
  |
8 |     for i in v {
  |              ^ `[u32]` is not an iterator; maybe try calling `.iter()` or a similar method
  |
  = help: the trait `std::iter::Iterator` is not implemented for `[u32]`
  = note: required by `std::iter::IntoIterator::into_iter`
```

Basically, there's no way to dereference a slice. It logically makes
sense in any event to just keep a reference to the block of memory
holding the values, whether it's on the stack, heap, or the executable
itself (like string literals, which we'll get to later).

## Deref

There's something fishy; why does the ampersand/borrow operator give
us different types? The following compiles just fine!

```rust
fn main() {
    let v = vec![1, 2, 3];
    let _: &Vec<u32> = &v;
    let _: &[u32] = &v;
}
```

It turns out that the borrow operator interacts with "`Deref`
coercion." If you're curious about this, please check out [the docs
for the `Deref`
trait](https://doc.rust-lang.org/std/ops/trait.Deref.html). As an
example, I can create a new struct which can be borrowed into a slice:

```rust
use std::ops::Deref;

struct MyArray([u32; 5]);

impl MyArray {
    fn new() -> MyArray {
        MyArray([42; 5])
    }
}

impl Deref for MyArray {
    type Target = [u32];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

fn main() {
    let ma = MyArray::new();
    let _: &MyArray = &ma;
    let _: &[u32] = &ma;
}
```

Thanks to udoprog for [answering this
question](https://stackoverflow.com/questions/53250856/can-i-borrow-a-custom-type-into-a-slice). Also,
just because you _can_ do this [doesn't necessarily mean you
should](https://stackoverflow.com/questions/45086595/is-it-considered-a-bad-practice-to-implement-deref-for-newtypes).

## Using slices

Slices are data types like any others. You can check out the
[`std::slice` module
documentation](https://doc.rust-lang.org/std/slice/index.html) and the
[`slice` primitive
type](https://doc.rust-lang.org/std/primitive.slice.html).

Some common ways to interact with them include:

* Using them as `Iterator`s
* Indexing them with `slice[idx]` syntax
* Taking subslices with `slice[start..end]` syntax

## Byte literals

If you put a lower case `b` in front of a string literal, you'll get a
byte array. You can either treat this as a fixed length array or, more
commonly, as a slice:

```rust
fn main() {
    let bytearray1: &[u8; 22] = b"Hello World in binary!";
    let bytearray2: &[u8] = b"Hello World in binary!";
    println!("{:?}", bytearray1);
    println!("{:?}", bytearray2);
}
```

Note that you always receive a _reference_ to the value, not the value
itself. The data is stored in the program executable itself, and
therefore cannot be modified (thus always receiving an immutable
reference).

__Exercise 4__ Add lifetime parameters to the `bytearray1` and
`bytearray2` types above.

## Strings

And finally we can talk about strings! You may think that a string
literal would be a fixed length array of `char`s. You can in fact
create such a thing:

```rust
fn main() {
    let char_array: [char; 5] = ['H', 'e', 'l', 'l', 'o'];
    println!("{:?}", char_array);
}
```

However, this is _not_ what a `str` is. The representation above is
highly inefficient. Since a `char` in Rust has full Unicode support,
it takes up 4 bytes in memory (32 bits). However, for most data, this
is overkill. A character encoding like UTF-8 will be far more
efficient.

__NOTE__ If you're not familiar with Unicode and character encodings,
it's safe to gloss over these details here. It's not vitally important
to understanding how strings work in Rust.

Instead, a string slice (`&str`) is essentially a newtype wrapper
around a byte slice (`&[u8]`), which is guaranteed to be in UTF-8
encoding. This has some important trade-offs:

* You can cheaply (freely?) convert from a `&str` to a `&[u8]`, which
  can be great for making system calls
* You cannot get O(1) random access within strings, since the UTF-8
  encoding doesn't allow for this. Instead, you need to work with a
  character iterator to view the individual characters.

__Exercise 5__ Use `std::env::args` and the `chars()` method on
`String` to print out the number of characters in each command line
arguments. Bonus points: also print out the number of bytes. Sample
usage:

```
$ cargo run שלום
arg: target/debug/foo, characters: 16, bytes: 16
arg: שלום, characters: 4, bytes: 8
```

Don't forget, the first argument is the name of the executable.

## Lifetimes in data structures

One final topic for today is lifetimes in data structures. It's
entirely possible to keep references in your data structures. However,
when you do so, you need to be explicit about their lifetimes. For
example, this will fail to compile:

```rust
struct Person {
    name: &str,
    age: u32,
}
```

Instead, you would need to write it as:

```rust
struct Person<'a> {
    name: &'a str,
    age: u32,
}
```

The general recommendation I've received, and which I'd pass on, is
avoid this when possible. Things end up getting significantly more
complicated when dealing with lifetime parameters in data
structures. Typically, you should use owned versions of values
(e.g. `String` instead of `&str`, or `Vec` or array instead of a
slice) inside your data structures. In such a case, you need to
ensure that the lifetime of the reference within the structure
outlives the structure itself.

There are times when you can avoid some extra cloning and allocation
if you use references in your data structure, and the time will
probably come when you need to do it. But I'd recommend waiting until
your profiling points you at a specific decision being the bottleneck.
For more information, see [the Rust book](https://doc.rust-lang.org/book/second-edition/ch10-03-lifetime-syntax.html#lifetime-annotations-in-struct-definitions).

## References and slices in APIs

Some general advice which I received and has mostly steered me
correctly is:

> When receiving parameters, prefer slices when possible

However, there are times when this is overly simplistic. If you want a
deeper dive, there a great blog post covering some trade-offs in
public APIs: [On dealing with owning and borrowing in public
interfaces](https://phaazon.net/blog/on-owning-borrowing-pub-interface). The
[Reddit
discussion](https://www.reddit.com/r/rust/comments/9tzygo/on_dealing_with_owning_and_borrowing_in_public/)
is also great.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
