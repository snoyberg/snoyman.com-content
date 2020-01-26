In this lesson, we're going to cover what I'm dubbing the "rule of
three," which applies to function parameters, iterators, and
closures. We've already seen this rule applied to function parameters,
but didn't discuss is so explicitly. We'll expand on parameters, and
use that to launch into new information on both iterators and
closures.

This post is part of a series based on [teaching Rust at FP
Complete](https://www.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Types of parameters

The first thing I want to deal with is a potential misconception. This
may be one of those "my brain has been scrambled by Haskell"
misconceptions that imperative programmers won't feel, so apologies if
I'm just humoring myself and other Haskellers.

Do these two functions have the same type signature?

```rust
fn foo(mut person: Person) { unimplemented!() }
fn bar(person: Person) { unimplemented!() }
```

The Haskeller in me screams "they're different!" However, they're
_exactly the same_. The _inner mutability_ of the `person` variable in
the function is _irrelevant_ to someone calling the function. The
caller of the function will move the `Person` value into the function,
regardless of whether the value can be mutated or not. We've already
seen a hint of this: the fact that we can pass an immutable value to a
function like `foo`:

```rust
fn main() {
    let alice = Person { name: String::from("Alice"), age: 30 };
    foo(alice); // it works!
}
```

With that misconception out of the way, let's consider two other
similar functions:

```rust
fn baz(person: &Person) { unimplemented!() }
fn bin(person: &mut Person) { unimplemented!() }
```

Firstly, it's pretty easy to say that both `baz` and `bin` have
different signatures than `foo`. These are taking references to a
`Person`, not a `Person` itself. But what about `baz` vs `bin`? Are
they the same or different? You may be tempted to follow the same
logic as `foo` vs `bar` and decide that the `mut` is an internal
detail of the function. But this isn't true! Observe:

```rust
fn main() {
    let mut alice = Person { name: String::from("Alice"), age: 30 };
    baz(&alice); // this works
    bin(&alice); // this fails
    bin(&mut alice); // but this works
}
```

The first call to `bin` will not compile, because `bin` requires a
_mutable_ reference, and we've provided an _immutable_ reference. We
need to use the second version of the call. And not only does this
have a _syntactic_ difference, but a _semantic_ difference as well:
we've taken a mutable reference, which means we can have no other
references at the same time (remember our borrow rules from lesson 2).

The upshot of this is that there are three different ways we can pass
a value into a function which appear at the type level:

* Pass by value (move semantics), like `foo`
* Pass by immutable reference, like `baz`
* Pass by mutable reference, like `bin`

In addition, orthogonally, the variable that captures that parameters
can itself be either immutable or mutable.

### Mutable vs immutable pass-by-value

This one is relatively easy to see. What extra functionality do we get
by having a mutable pass-by-value? The ability to mutate the value of
course! Let's look at two different ways of implementing a birthday
function, which increases someone's age by 1.

```rust
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

fn birthday_immutable(person: Person) -> Person {
    Person {
        name: person.name,
        age: person.age + 1,
    }
}

fn birthday_mutable(mut person: Person) -> Person {
    person.age += 1;
    person
}

fn main() {
    let alice1 = Person { name: String::from("Alice"), age: 30 };
    println!("Alice 1: {:?}", alice1);
    let alice2 = birthday_immutable(alice1);
    println!("Alice 2: {:?}", alice2);
    let alice3 = birthday_mutable(alice2);
    println!("Alice 3: {:?}", alice3);
}
```

Some important takeaways:

* Our `_immutable` implementation follows a more functional idiom,
  creating a new `Person` value by deconstructing the original
  `Person` value. This works just fine in Rust, but is not idiomatic,
  and potentially less efficient.
* We call both versions of this function in exactly the same way,
  reinforcing the claim that these two functions have the same
  signature.
* You cannot reuse the `alice1` or `alice2` values in `main`, since
  they've been moved during their calls.
* `alice2` is an immutable variable, but it still gets passed in to a
  function which mutates it.

### Mutable vs immutable pass-by-mutable-reference

This one already gets significantly harder to observe, which indicates a simple fact of Rust: _it's unusual to want a mutable variable for references_. The example below is very contrived, and requires playing with the more advanced concept of explicit lifetime parameters to even make it make sense. But it does demonstrate the difference between where the `mut` appears.

Before we dive in: parameters that begin with a single quote (`'`) are
_lifetime parameters_, and indicate how long a reference needs to
live. In the examples below, we're saying "the two references must
have the same lifetime." We won't cover this in more detail here, at
least not yet. If you want to learn about lifetimes, please [check out
the Rust
book](https://doc.rust-lang.org/book/2018-edition/ch10-03-lifetime-syntax.html).

OK, let's see a difference between an immutable variable holding a
mutable reference and a mutable variable holding a mutable reference!

```rust
#[derive(Debug)]
struct Person {
    name: String,
    age: u32,
}

fn birthday_immutable(person: &mut Person) {
    person.age += 1;
}

fn birthday_mutable<'a>(mut person: &'a mut Person, replacement: &'a mut Person) {
    person = replacement;
    person.age += 1;
}

fn main() {
    let mut alice = Person { name: String::from("Alice"), age: 30 };
    let mut bob = Person { name: String::from("Bob"), age: 20 };
    println!("Alice 1: {:?}, Bob 1: {:?}", alice, bob);
    birthday_immutable(&mut alice);
    println!("Alice 2: {:?}, Bob 2: {:?}", alice, bob);
    birthday_mutable(&mut alice, &mut bob);
    println!("Alice 3: {:?}, Bob 3: {:?}", alice, bob);
}

// does not compile
fn birthday_immutable_broken<'a>(person: &'a mut Person, replacement: &'a mut Person) {
    person = replacement;
    person.age += 1;
}
```

`birthday_immutable` is fairly simple. We have a mutable reference,
and we've stored it in an immutable variable. We've completely free to
mutate the value pointed to by that reference. The takeaway is: we're
mutating the value, _not_ the variable, which is remaining the same.

`birthday_mutable` is a contrived, ugly mess, but it demonstrates our
point. Here, we take _two_ references: a `person`, and a
`replacement`. They're both mutable references, but `person` is in a
mutable variable. The first thing we do is `person =
replacement;`. This changes what our `person` variable is pointing at,
and _does not modify_ the original value being pointed at by the
reference at all. In fact, when compiling this, we'll get a warning
that we never used the value passed to `person`:

```
warning: value passed to `person` is never read
```

Notice that we needed to mark both `alice` and `bob` as mutable in
`main` in this example. That's because we pass them by mutable
reference, which requires that we have the ability to mutate
them. This is different from pass-by-value with move semantics,
because in our `main` function, we can directly observe the effect of
mutating the references we've passed in.

Also notice that we also have a `birthday_immutable_broken`
version. As you may guess from the name, it doesn't compile. We cannot
change what `person` points to if it is an immutable variable.

__Challenge__ Figure out what the output of this program is going to
be before you run it.

### Mutable vs immutable pass-by-immutable-reference

I'm not actually going to cover this case, since it's basically the
same as the previous one. If you mark a variable as mutable, you can
change which reference it holds. Feel free to play around with an
example like the one above using immutable references.

### Mutable to immutable

Let's point out one final bit:

```rust
fn needs_mutable(x: &mut u32) {
    *x *= 2;
}

fn needs_immutable(x: &u32) {
    println!("{}", x);
}

fn main() {
    let mut x: u32 = 5;
    let y: &mut u32 = &mut x;
    needs_immutable(y);
    needs_mutable(y);
    needs_immutable(y);
}
```

From what I've told you so far, you should expect this program to fail
to compile. `y` is of type `&mut u32`, but we're passing it to
`needs_immutable` which requires a `&u32`. Type mismatch, go home!

Not so fast: since the guarantees of a mutable reference are strictly
stronger than an immutable reference, you can always use a mutable
reference where an immutable was needed. (Hold onto this, it will be
important for closures below.)

### Summary of the rule of three for parameters

There are three types of parameters:

* Pass by value
* Pass by immutable reference
* Pass by mutable reference

This is what I'm calling the rule of three. The captured variables
within a function can either be mutable or immutable, which is
orthogonal to the type of the parameter. However, it's by far most
common to have a mutable variable with a pass-by-value. Also, at the
call site, a variable must be mutable if it is called on a pass by
mutable reference functions. Finally, you can use a mutable reference
where an immutable was requested.

## Exercise 1

Fix the program below so that it outputs the number 10. Ensure that
there are no compiler warnings.

```rust
fn double(mut x: u32) {
    x *= 2;
}

fn main() {
    let x = 5;
    double(x);
    println!("{}", x);
}
```

Hint: you'll need to know how to _dereference_ a reference, by putting
a asterisk (`*`) in front of the variable.

## Iterators

What's the output of the program below?

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in nums {
        println!("{}", i);
    }
}
```

That's right: it prints the numbers 1 to 5. How about this one?

```rust
fn main() {
    for i in 1..3 {
        let nums = vec![1, 2, 3, 4, 5];
        for j in nums {
            println!("{},{}", i, j);
        }
    }
}
```

It prints `1,1`, `1,2`, ..., `1,5`, `2,1`, ..., `2,5`. Cool, easy
enough. Let's move `nums` a bit. What does this do?

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        for j in nums {
            println!("{},{}", i, j);
        }
    }
}
```

Trick question: it doesn't compile!

```
error[E0382]: use of moved value: `nums`
 --> main.rs:4:18
  |
4 |         for j in nums {
  |                  ^^^^ value moved here in previous iteration of loop
  |
  = note: move occurs because `nums` has type `std::vec::Vec<i32>`, which does not implement the `Copy` trait

error: aborting due to previous error
```

Well, that kind of makes sense. The first time we run through the
outer loop, we _move_ the `nums` value into the inner loop. Then, we
can't use the `nums` value again on the second pass through the
loop. OK, logical.

__Side note__ This was one of my personal "mind blown" moments with
Rust, realizing how sophisticated lifetime tracking was to work
through loops like this. Rust is pretty amazing.

We can go back to our previous version and put `nums` inside the first
`for` loop. That means recreating the value each time we pass through
that outer `for` loop. For our little vector example, it's not a big
deal. But imagine constructing `nums` was expensive. This would be a
major overhead!

If we want to avoid the move of `nums`, can we get away with just
borrowing it instead? Yes we can!

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        for j in &nums {
            println!("{},{}", i, j);
        }
    }
}
```

This works, but I've got a question for you: what's the type of `j`?
I've got a sneaky little trick to test out different options. If you
throw this in just above the `println!` call, you'll get an error
message:

```rust
let _: u32 = j;
```

However, this will compile just fine:

```rust
let _: &u32 = j;
```

By iterating over a reference to `nums`, we got a reference to each
value instead of the value itself. That makes sense. Can we complete
our "rule of three" with mutable references? Yet again, yes!

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        for j in &mut nums {
            let _: &mut u32 = j;
            println!("{},{}", i, j);
            *j *= 2;
        }
    }
}
```

__Challenges__ First, there's a compilation error in the program above. Try to catch
it before asking the compiler to help. Second, guess the output of
this program before running it.

Our rule of three translates into iterators as well! We have can
iterators of values, iterators of references, and iterators of mutable
references. Sweet!

### New nomenclature

The `Vec` struct has three different methods on it that are relevant to our examples above. Starting with the mutable case, we can replace the line:

```rust
for j in &mut nums {
```

with

```rust
for j in nums.iter_mut() {
```

The signature of that method is:

```rust
pub fn iter_mut(&mut self) -> IterMut<T>
```

Similarly, we've got a `iter()` method that can replace our immutable
reference case:

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        for j in nums.iter() {
            let _: &u32 = j;
            println!("{},{}", i, j);
        }
    }
}
```

And, finally, what about the iterator of values case? There, the
nomenclature is `into_iter`. The idea is that we are _converting_ the
existing value _into_ an iterator, consuming the previous value (the
`Vec` in this case) completely. This code won't compile, go ahead and
fix it by moving the `let nums` statement.

```rust
fn main() {
    let nums = vec![1, 2, 3, 4, 5];
    for i in 1..3 {
        for j in nums.into_iter() {
            println!("{},{}", i, j);
        }
    }
}
```

### Reexamining for loops

Here's a cool little trick I didn't mention before. `for` loops are a
bit more flexible than I'd implied. The `into_iter` method I mention
is actually part of a trait, appropriately named
[`IntoIterator`](https://doc.rust-lang.org/std/iter/trait.IntoIterator.html). Whenever
you use `for x in y`, the compiler automatically calls `into_iter()`
on `y`. This allows you to loop over types which don't actually have
their own implementation of `Iterator`.

### Exercise 2

Make this program compile by defining an `IntoIterator`
implementation for `InfiniteUnit`. Do _not_ define an `Iterator`
implementation for it. You'll probably want to define an extra
datatype. (Extra credit: also try to find a helper function in the
standard library that repeats values.)

```rust
struct InfiniteUnit;

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

### Summary of the rule of three for iterators

Just like function parameters, iterators come in three flavors,
corresponding to the following naming scheme:

* `into_iter()` is an iterator of values, with move semantics
* `iter()` is an iterator of immutable references
* `iter_mut()` is an iterator of mutable references

Only `iter_mut()` requires that the original variable itself be mutable.

## Closures

We've danced around closures a bit throughout the crash course so
far. Closures are like functions, in that they can be called on some
arguments. Closures are unlike functions in that they can capture
values from the local scope. We'll demonstrate this in an example,
after a word of warning.

__One word of warning__ If you're coming from a non-functional
programming background, you'll likely find closures in Rust very
powerful, and surprisingly common in library usage. If you come from a
functional programming background, you'll likely be annoyed at how
much you have to think about ownership of data when working with
closures. As a Haskeller, this is still the aspect of Rust I most
often get caught on. I promise, the trade-offs in the design are
logical and necessary to achieve Rust's goals, but it can feel a bit
onerous when compared to Haskell, or even compared to Javascript.

Alright, back to our function vs closure thing. Did you know that you can define a function _inside another function_?

```rust
fn main() {
    fn say_hi() {
        let msg: &str = "Hi!";
        println!("{}", msg);
    };
    say_hi();
    say_hi();
}
```

That's pretty nifty. Let's slightly refactor that:

```rust
fn main() {
    let msg: &str = "Hi!";
    fn say_hi() {
        println!("{}", msg);
    };
    say_hi();
    say_hi();
}
```

Unfortunately, Rust doesn't like that very much:

```
error[E0434]: can't capture dynamic environment in a fn item
 --> main.rs:4:24
  |
4 |         println!("{}", msg);
  |                        ^^^
  |
  = help: use the `|| { ... }` closure form instead

error: aborting due to previous error
```

Fortunately, the compiler tells us _exactly_ how to fix it: use a
closure! Let's rewrite that:

```rust
fn main() {
    let msg: &str = "Hi!";
    let say_hi = || {
        println!("{}", msg);
    };
    say_hi();
    say_hi();
}
```

We now have a closure (introduced by `||`) which takes 0
arguments. And everything just works.

__Note__ You can shorten this a bit with `let say_hi = || println!("{}", msg);`,
which is more idiomatic.

__Exercise 3__ Rewrite the above so that instead of taking 0
arguments, `say_hi` takes a single argument: the `msg` variable. Then
try out the `fn` version again.

## The type of a closure

What exactly is the type of `say_hi`? I'm going to use an ugly trick
to get the compiler to tell us: give it the _wrong_ type, and then try
to compile. It's probably safe to assume that a closure isn't a `u32`,
so let's try this:

```rust
fn main() {
    let msg: &str = "Hi!";
    let say_hi: u32 = |msg| println!("{}", msg);
}
```

And we get the error message:

```rust
error[E0308]: mismatched types
 --> main.rs:3:23
  |
3 |     let say_hi: u32 = |msg| println!("{}", msg);
  |                       ^^^^^^^^^^^^^^^^^^^^^^^^^ expected u32, found closure
  |
  = note: expected type `u32`
             found type `[closure@main.rs:3:23: 3:48]`

error: aborting due to previous error

For more information about this error, try `rustc --explain E0308`.
```

`[closure@main.rs:3:23: 3:48]` looks like a weird type... but let's
just give it a shot and see what happens:

```rust
fn main() {
    let msg: &str = "Hi!";
    let say_hi: [closure@main.rs:3:23: 3:48] = |msg| println!("{}", msg);
}
```

But the compiler shoots us down:

```
error: expected one of `!`, `(`, `+`, `::`, `;`, `<`, or `]`, found `@`
 --> main.rs:3:25
  |
3 |     let say_hi: [closure@main.rs:3:23: 3:48] = |msg| println!("{}", msg);
  |         ------          ^ expected one of 7 possible tokens here
  |         |
  |         while parsing the type for `say_hi`

error: aborting due to previous error
```

Oh well, that isn't a valid type. What exactly is the compiler telling
us then?

## Anonymous types

The types of closures are anonymous in Rust. We cannot directly refer
to them at all. But this leaves us in a bit of a pickle. What if we
want to pass a closure into another function? For example, let's try
out this program:

```rust
fn main() {
    let say_message = |msg: &str| println!("{}", msg);
    call_with_hi(say_message);
    call_with_hi(say_message);
}

fn call_with_hi<F>(f: F) {
    f("Hi!");
}
```

We've added a type annotation on the `msg` parameter in the
closure. These are generally optional in closures, unless type
inference fails. And with our current broken code, type inference is
definitely failing. We're including it now to get better error
messages later.

We also now have a type parameter, called `F`, for the closure we're
passing in. We don't know anything about `F` right now, but we're
going to just try using it in a function call manner. If we compile
this, we get:

```
error[E0618]: expected function, found `F`
 --> main.rs:8:5
  |
7 | fn call_with_hi<F>(f: F) {
  |                    - `F` defined here
8 |     f("Hi!");
  |     ^^^^^^^^ not a function

error: aborting due to previous error

For more information about this error, try `rustc --explain E0618`.
```

OK, fair enough: the compiler doesn't know that `F` is a function. It's time to finally introduce the magic that will make this compile: the `Fn` trait!

```rust
fn call_with_hi<F>(f: F)
    where F: Fn(&str) -> ()
{
    f("Hi!");
}
```

We've now put a constraint on `F` that it must be a function, which
takes a single argument of type `&str`, and returns a unit
value. Actually, returning unit values is the default, so we can just
omit that bit:

```rust
fn call_with_hi<F>(f: F)
    where F: Fn(&str)
{
    f("Hi!");
}
```

Another nifty thing about the `Fn` trait is that it doesn't just apply
to closures. It works on regular ol' functions too:

> __Exercise 4__ Rewrite `say_message` as a function _outside_ of `main` and make the program above compile.

This was a bit boring, since `say_message` isn't actually a
closure. Let's change that a bit.

```rust
fn main() {
    let name = String::from("Alice");
    let say_something = |msg: &str| println!("{}, {}", msg, name);
    call_with_hi(say_something);
    call_with_hi(say_something);
    call_with_bye(say_something);
    call_with_bye(say_something);
}

fn call_with_hi<F>(f: F)
    where F: Fn(&str)
{
    f("Hi");
}

fn call_with_bye<F>(f: F)
    where F: Fn(&str)
{
    f("Bye");
}
```

## Mutable variables

Remember the good old days of visitor counters on webpages? Let's
recreate that beautiful experience!

```rust
fn main() {
    let mut count = 0;

    for _ in 1..6 {
        count += 1;
        println!("You are visitor #{}", count);
    }
}
```

That works, but it's so boring! Let's make it more interesting with a
closure.

```rust
fn main() {
    let mut count = 0;
    let visit = || {
        count += 1;
        println!("You are visitor #{}", count);
    };

    for _ in 1..6 {
        visit();
    }
}
```

The compiler disagrees:

```
error[E0596]: cannot borrow immutable local variable `visit` as mutable
 --> main.rs:9:9
  |
3 |     let visit = || {
  |         ----- help: make this binding mutable: `mut visit`
...
9 |         visit();
  |         ^^^^^ cannot borrow mutably

error: aborting due to previous error

For more information about this error, try `rustc --explain E0596`.
```

Huh... what? Apparently calling a function counts as borrowing
it. Fine, that explains why we're allowed to call it multiple
times. But now we need to borrow it _mutably_ for some reason. How
come?

That reason is fairly simple: `visit` has captured and is mutating a
local variable, `count`. Therefore, any borrow of it is implicitly
mutably borrowing `count` as well. Logically, this makes sense. But
how about at the type level? How is the compiler tracking this
mutability? To see that, let's extend this a bit further with a helper
function:

```rust
fn main() {
    let mut count = 0;
    let visit = || {
        count += 1;
        println!("You are visitor #{}", count);
    };

    call_five_times(visit);
}

fn call_five_times<F>(f: F)
where
    F: Fn(),
{
    for _ in 1..6 {
        f();
    }
}
```

We get the error message:

```
error[E0525]: expected a closure that implements the `Fn` trait, but this closure only implements `FnMut`
```

Nice! Rust has two different traits for functions: one which covers
functions that don't mutate their environment (`Fn`), and one for
functions which do mutate their environment (`FnMut`). Let's try
modifying our `where` to use `FnMut` instead. We get one more error
message:

```
error[E0596]: cannot borrow immutable argument `f` as mutable
  --> main.rs:16:9
   |
11 | fn call_five_times<F>(f: F)
   |                       - help: make this binding mutable: `mut f`
...
16 |         f();
   |         ^ cannot borrow mutably

error: aborting due to previous error

For more information about this error, try `rustc --explain E0596`.
```

Calling this mutating function requires taking a mutable borrow of the
variable, and that requires defining the variable as mutable. Go ahead
and stick a `mut` in front of the `f: F`, and you'll be golden.

## Multiple traits?

Is this closure a `Fn` or a `FnMut`?

```rust
|| println!("Hello World!");
```

Well, it doesn't modify any variables in the local scope, so
presumably it's an `Fn`. Therefore, passing it to
`call_five_times`&mdash;which expects a `FnMut`&mdash;should fail,
right? Not so fast, it works just fine! Go ahead and add this line to
the program above and prove it to yourself:

```rust
call_five_times(|| println!("Hello World!"));
```

Every value which is a `Fn` is _automatically_ an `FnMut`. This is
similar to what happens with a function parameter: if you have a
mutable reference, you can automatically use it as an immutable
reference, since the guarantees of a mutable reference are
stronger. Similarly, if we're using a function in such a way that it's
safe even if the function is mutable (`FnMut`), it's certainly safe to
do the same thing with an immutable function (`Fn`).

Does this sound a bit like subtyping? Good, it should :)

## The rule of three?

If you've noticed, we now have two different types of functions, in a
lesson entitled "the rule of three." What could possibly be coming
next? We've seen functions that can be called multiple times in an
immutable context, kind of like immutable references. We've seen
functions that can be called multiple times in a mutable context, kind
of like mutable references. That just leaves one thing... call by
value/move semantics!

We're going to define a closure that moves a local variable
around. We're going to go back to use a `String` instead of a
`u32`, to avoid the fact that a `u32` is `Copy`able. And we're going
to use a weird bit of magic in the middle to force things to be moved
instead of being treated as references. We'll go into gory detail on
that trick later, and see alternatives.

```rust
fn main() {
    let name = String::from("Alice");

    let welcome = || {
        let name = name; // here's the magic
        println!("Welcome, {}", name);
    };

    welcome();
}
```

Alright, `name` is moved into the `welcome` closure. This is forced
with the `let name = name;` bit. Still not 100% convinced that `name`
was actually moved in? Watch this:

```rust
fn main() {
    let name1 = String::from("Alice");

    let welcome = || {
        let mut name2 = name1;
        name2 += " and Bob";
        println!("Welcome, {}", name2);
    };

    welcome();
}
```

`name1` is defined as _immutable_. But `name2` is mutable, and we do
in fact successfully mutate it. This can only happen if we pass by
value instead of by reference. Want further proof? Try to use `name1`
again after we've defined `welcome`.

## The third function trait

Let's complete our rule of three. Remember our `call_five_times`?
Let's use it on `welcome`:

```rust
fn main() {
    let name = String::from("Alice");

    let welcome = || {
        let mut name = name;
        name += " and Bob";
        println!("Welcome, {}", name);
    };

    call_five_times(welcome);
}

fn call_five_times<F>(f: F)
where
    F: Fn(),
{
    for _ in 1..6 {
        f();
    }
}
```

And we get a brand new error message, this time referencing `FnOnce`:

```
error[E0525]: expected a closure that implements the `Fn` trait, but this closure only implements `FnOnce`
  --> main.rs:4:19
   |
4  |     let welcome = || {
   |                   ^^ this closure implements `FnOnce`, not `Fn`
5  |         let mut name = name;
   |                        ---- closure is `FnOnce` because it moves the variable `name` out of its environment
...
10 |     call_five_times(welcome);
   |     --------------- the requirement to implement `Fn` derives from here

error: aborting due to previous error

For more information about this error, try `rustc --explain E0525`.
```

Replacing `Fn()` with `FnOnce()` should fix the compilation, right? Wrong!

```
error[E0382]: use of moved value: `f`
  --> main.rs:18:9
   |
18 |         f();
   |         ^ value moved here in previous iteration of loop
   |
   = note: move occurs because `f` has type `F`, which does not implement the `Copy` trait

error: aborting due to previous error

For more information about this error, try `rustc --explain E0382`.
```

Our loop ends up calling `f` multiple times. But each time we call
`f`, we're moving the value. Therefore, the function can _only be
called once_. Maybe that's why they named it `FnOnce`.

Let's rewrite this to have a helper function that only calls things
once:

```rust
fn main() {
    let name = String::from("Alice");

    let welcome = || {
        let mut name = name;
        name += " and Bob";
        println!("Welcome, {}", name);
    };

    call_once(welcome);
}

fn call_once<F>(f: F)
where
    F: FnOnce(),
{
    f();
}
```

That works just fine. Hurrah!

## Further function subtyping

Previously, we said that every `Fn` is also an `FnMut`, since anywhere
you can safely call a mutable function, you can also call an immutable
function. It turns out that every `Fn` and every `FnMut` are also
`FnOnce`s, because any context you can guarantee the function will
only be called once is safe for running functions with mutable or
immutable environments.

## The move keyword

There's a subtle point we're about to get into, which I [didn't
understand till I wrote this
lesson](https://stackoverflow.com/q/53029622/369198) (thanks to Sven
Marnach for the explanation there). The [Rust by Example section on closures](https://doc.rust-lang.org/rust-by-example/fn/closures.html)
was the best resource for helping it all click for me. I'll do my best
here explaining it myself.

Functions accept parameters explicitly, complete with type
signatures. You're able to explicitly state whether a parameter is
pass by value, mutable reference, or immutable reference. Then, when
you use it, you're able to choose any of the weaker forms
available. For example, if you pass a parameter by mutable reference,
you can later use it by immutable reference. However, you _cannot_ use
it by value:

```rust
fn pass_by_value(_x: String) {}
fn pass_by_ref(_x: &String) {}

fn pass_by_mut_ref(x: &mut String) {
    pass_by_ref(x); // that's fine
    pass_by_value(*x); // that's a paddlin'
}

fn main() {}
```

Closures accept parameters, but they make the type annotations
optional. If you omit them, they are implicit. In addition, closures
allow you to capture variables. These never take a type annotation;
they are _always_ implicit. Nonetheless, there needs to be some
concept of how these values were captured, just like we need to know
how parameters are passed into a function.

How a value is captured will imply the same set of borrow rules we're
used to in Rust, in particular:

* If by reference, then other references can live concurrently with
  the closure
* If by mutable reference, then as long as the closure is alive, no
  other references to the values can exist. However, once the closure
  is dropped, other references can exist again.
* If by value, then the value cannot be used by anything ever
  again. (This automatically implies that the closure _owns_ the
  value.)

However, there's an important and (dare I repeat myself) subtle
distinction between closures and functions:

__Closures can own data, functions cannot__

Sure, if you pass by value to a function, the function call takes
ownership of the data during execution. But closures are different:
the closure _itself_ can own data, and use it while it is being
called. Let's demonstrate:

```rust
fn main() {
    // owned by main
    let name_outer = String::from("Alice");

    let say_hi = || {
        // force a move, again, we'll get smarter in a second
        let name_inner = name_outer;
        println!("Hello, {}", name_inner);
    };

    // main no longer owns name_outer, try this:
    println!("Using name from main: {}", name_outer); // error!

    // but name_inner lives on, in say_hi!
    say_hi(); // success
}
```

Try as you might, you could not achieve the same thing with a plain
old function, you'd need to keep `name_outer` alive separately and
then pass it in.

Alright, let's get to that smarter way to force a move. In the closure
above, we have `let name_inner = name_outer;`. This forces the closure
to use `name_outer` by value. Since we use by value, we can only call
this closure once, since it fully consumes `name_outer` on the first
call. (Go ahead and try adding a second `say_hi()` call.) But in
reality, we're only using the name by immutable reference inside the
closure. We _should_ be able to call it multiple times. If we skip the
forced use by value, we can use by reference, leaving the `name_outer`
in the original scope:

```rust
fn main() {
    // owned by main
    let name_outer = String::from("Alice");

    let say_hi = || {
        // use by reference
        let name_inner = &name_outer;
        println!("Hello, {}", name_inner);
    };

    // main still owns name_outer, this is fine
    println!("Using name from main: {}", name_outer); // success

    // but name_inner lives on, in say_hi!
    say_hi(); // success
    say_hi(); // success
}
```

However, if we change things around a bit, so that `name_outer` goes
out of scope before `say_hi`, everything falls apart!

```rust
fn main() {
    let say_hi = { // forcing the creation of a smaller scope
        // owned by the smaller scope
        let name_outer = String::from("Alice");

        // doesn't work, closure outlives captured values
        || {
            // use by reference
            let name_inner = &name_outer;
            println!("Hello, {}", name_inner);
        }
    };

    // syntactically invalid, name_outer isn't in this scope
    //println!("Using name from main: {}", name_outer); // error!

    say_hi();
    say_hi();
}
```

What we need is some way to say: I'd like the closure to own the
values it captures, but I don't want to have to force a use by value
to do it. That will allow a closure to outlive the original scope of
the value, but still allow a closure to be called multiple times. And
to do that, we introduce the `move` keyword:

```rust
fn main() {
    let say_hi = { // forcing the creation of a smaller scope
        // owned by the smaller scope
        let name_outer = String::from("Alice");

        // now it works!
        move || {
            // use by reference
            let name_inner = &name_outer;
            println!("Hello, {}", name_inner);
        }
    };

    // syntactically invalid, name_outer isn't in this scope
    //println!("Using name from main: {}", name_outer); // error!

    say_hi();
    say_hi();
}
```

The ownership of `name_outer` passes from the original scope to the
closure itself. We still only use it by reference, and therefore we
can call it multiple times. Hurrah!

One final bit here. Using the `move` keyword like this moves all
captured variables into the closure, and therefore they cannot be used
after the closure. For example, this will fail to compile:

```rust
fn main() {
    let name = String::from("Alice");
    let _ = move || println!("Hello, {}", name);
    println!("Using name from main: {}", name); // error!
}
```

## Reluctant Rust

Alright, one final point before we sum things up and dive into
examples. The type of capture is implicit in a closure. How does Rust
decide whether to capture by value, mutable reference, or immutable
reference. I like to think of Rust as being reluctant here: it strives
to capture the weakest way possible. To paraphrase the Rust by Example
book:

__Closures will preferentially capture by immutable reference, then by
mutable reference, and only then by value.__

In our previous examples with `let name_inner = name_outer;`, we
forced Rust to capture by value. However, it doesn't like doing that,
and will instead capture by reference (mutable or immutable) if it can
get away with that. It does this based on the strongest kind of usage
for that value. That is:

* If any part of the closure uses a variable by value, it must be
  captured by value.
* Otherwise, if any part of the closure uses a variable by mutable
  reference, it must be captured by mutable reference.
* Otherwise, if any part of the closure uses a variable by immutable
  reference, it must be captured by immutable reference.

It does this reluctant capturing _even if it causes the program to
fail to compile_. Capturing by reference instead of value can cause
lifetime issues, as we've seen previously. However, Rust does not look
at the full context of the usage of the closure to determine how to
capture, it only looks at the body of the closure itself.

But, since there are many legitimate cases where we want to force a
capture by value to solve lifetime issues, we have the `move` keyword
to force the issue.

__Side note__ It may be a little annoying at times that Rust doesn't
just look at your program as a whole and guess that you want that
`move` added. However, I think it's a great decision in the language:
that kind of "do what I mean" logic is fragile and often times
surprising.

## Recap: ownership, capture, and usage

To recap the salient points:

* Within a closure, a variable can be used by value, mutable
  reference, or immutable reference
* In addition, all variables captured by a closure can be captured by
  value, by mutable reference, or by immutable reference
* We cannot use a variable in a stronger way than it was captured. If
  it was captured by mutable reference, it can be used by immutable
  reference, but not by value.
* To solve lifetime issues, we can force a closure to capture by value
  with the `move` keyword.
* Short of the `move` keyword, Rust will be reluctant, and capture in
  the weakest way allowed by the body of the closure.
* Regarding the traits of closures:
    * If a closure uses anything by value, then the closure is a
      `FnOnce`
    * Otherwise, if a closure uses anything by mutable reference, then
      the closure is a `FnMut`, which automatically implies `FnOnce`
      as well
    * Otherwise, a closure is a `Fn`, which automatically implies both
      `FnMut` and `FnOnce`

I consider the points above complicated enough that I'm included a
number of further examples to help hammer the points home. These are
inspired heavily by the Rust by Example examples.

For all of the examples below, I'm going to assume the presence of the
following three helper functions in the source:

```rust
fn call_fn<F>(f: F) where F: Fn() {
    f()
}

fn call_fn_mut<F>(mut f: F) where F: FnMut() {
    f()
}

fn call_fn_once<F>(f: F) where F: FnOnce() {
    f()
}
```

### Examples

Consider this `main` function:

```rust
fn main() {
    let name = String::from("Alice");
    let say_hi = || println!("Hello, {}", name);
    call_fn(say_hi);
    call_fn_mut(say_hi);
    call_fn_once(say_hi);
}
```

`name` lives longer than `say_hi`, and therefore there's no problem
with the closure keeping an immutable reference to `name`. Since it
only has immutable references to the environment and consumes no
values, `say_hi` is a `Fn`, `FnMut`, and `FnOnce`, and the code above
compiles.

```rust
// bad!
fn main() {
    let say_hi = {
        let name = String::from("Alice");
        || println!("Hello, {}", name)
    };
}
```

By contrast, this example won't compile. `name` will go out of scope
once we leave the curly braces. However, our closure is capturing it
by reference, and so the reference outlives value. We could do our
trick from before of forcing it to capture by value:

```rust
fn main() {
    let say_hi = {
        let name = String::from("Alice");
        || {
            let name = name;
            println!("Hello, {}", name)
        }
    };
    //call_fn(say_hi);
    //call_fn_mut(say_hi);
    call_fn_once(say_hi);
}
```

But this only implements a `FnOnce`, since the value is captured and
consumed, preventing it from being run again. There's a better way!
Instead, we can force the closure to take ownership of `name`, but
still capture by reference:

```rust
fn main() {
    let say_hi = {
        let name = String::from("Alice");
        move || println!("Hello, {}", name)
    };
    call_fn(&say_hi);
    call_fn_mut(&say_hi);
    call_fn_once(&say_hi);
}
```

Now we're back to having a `Fn`, `FnMut`, and `FnOnce`! To avoid the
`say_hi` value itself from being moved with each call, we now pass a
reference to the `call_fn` functions. I believe (though am not 100%
certain) that this wasn't necessary in the first example since, above,
there was no captured environment, and therefore the closure could be
`Copy`ed. This closure, with a captured environment, cannot be
`Copy`ed`.

```rust
fn main() {
    let say_hi = {
        let name = String::from("Alice");
        || std::mem::drop(name)
    };
    //call_fn(say_hi);
    //call_fn_mut(say_hi);
    call_fn_once(say_hi);
}
```

This example uses the `drop` function to consume `name`. Since we use
by value, we must capture by value, and therefore must take ownership
of the value. As a result, sticking `move` at the front of the closure
is unnecessary, though it will do no harm.

```rust
fn main() {
    let mut say_hi = {
        let mut name = String::from("Alice");
        move || {
            name += " and Bob";
            println!("Hello, {}", name);
        }
    };
    //call_fn(say_hi);
    call_fn_mut(&mut say_hi);
    call_fn_once(&mut say_hi);
}
```

Using the `+=` operator on a `String` requires a mutable reference, so
we're out of the territory of immutable reference capturing. Rust
will fall back to capturing via mutable reference. That requires that
the `name` also be declared mutable. And since `name` will go out of
scope before the closure, we need to `move` ownership to the
closure. And since calling `say_hi` will mutate data, we need to put a
`mut` on its declaration too.

When we pass `say_hi` to the call functions, we need to use `&mut` to
ensure (1) the value isn't moved, and (2) the value can be
mutated. Also, `call_fn` is invalid here, since our closure is `FnMut`
and `FnOnce`, but _not_ `Fn`.

__Challenge__ What will the output of this program be? How many times
do we add the string `" and Bob"` to `name`?

```rust
fn main() {
    let mut name = String::from("Alice");
    let mut say_hi = || {
        name += " and Bob";
        println!("Hello, {}", name);
    };
    //call_fn(say_hi);
    call_fn_mut(&mut say_hi);
    call_fn_once(&mut say_hi);
}
```

We can also avoid the capture by letting the `name` live longer than the closure.

```rust
// bad!
fn main() {
    let mut name = String::from("Alice");
    let mut say_hi = || {
        name += " and Bob";
        println!("Hello, {}", name);
    };
    //call_fn(say_hi);
    call_fn_mut(&mut say_hi);
    call_fn_once(&mut say_hi);

    println!("And now name is: {}", name);
}
```

Adding the `println!` at the end, which references `name`, is invalid,
since `say_hi` is still in scope. This is due to _lexical
lifetimes_. You can turn on the (at time of writing) experimental
feature non-lexical lifetimes by adding `#![feature(nll)]` to the top
of your source code. Or, you can explicitly use braces to denote the
scope of the closure:

```rust
fn main() {
    let mut name = String::from("Alice");
    {
        let mut say_hi = || {
            name += " and Bob";
            println!("Hello, {}", name);
        };
        //call_fn(say_hi);
        call_fn_mut(&mut say_hi);
        call_fn_once(&mut say_hi);
    }

    println!("And now name is: {}", name);
}
```

You can also (perhaps somewhat obviously) use a value in multiple
different ways:

```rust
fn main() {
    let mut name = String::from("Alice");
    let mut say_hi = || {
        println!("Hello, {}", name); // use by ref
        name += " and Bob"; // use by mut ref
        std::mem::drop(name); // use by value
    };
    //call_fn(say_hi);
    //call_fn_mut(say_hi);
    call_fn_once(say_hi);
}
```

In these cases, the most powerful use determines the kind of capture
we need. Since we used by value above, we must also capture by value,
and therefore must take ownership.

## Which trait to use?

It may be intimidating to try and think through which of these three
traits you need. You can usually punt on this and let the compiler
yell at you. To quote [the Rust
book](https://doc.rust-lang.org/book/2018-edition/ch13-01-closures.html#capturing-the-environment-with-closures):

> Most of the time when specifying one of the `Fn` trait bounds, you
> can start with `Fn` and the compiler will tell you if you need
> `FnMut` or `FnOnce` based on what happens in the closure body.

I'd give a slightly different piece of advice, following the dictum of
"be lenient in what you accept." When receiving functions as
arguments, the most lenient thing to start with is a `FnOnce`. If your
usage turns out to be more restrictive, then listen to the
compiler.

For more information on closures as output parameters, see [Rust by Example's
chapter](https://doc.rust-lang.org/rust-by-example/fn/closures/output_parameters.html).

## Summary of the rule of three for closures

Both functions and closures are annotated using the `Fn` family of
trait bounds. These form a subtyping relationship, where every `Fn` is
also an `FnMut`, and every `FnMut` is also an `FnOnce`.

* `FnOnce` works like pass by value
* `FnMut` works like pass by mutable reference
* `Fn` works like pass by immutable reference

How these captured variables are used by the closure determines which
of these three it is. Since functions, by definition, never capture
local variables, they are always `Fn`.

## Exercise 5

Putting together what we've learned about iterators and closures,
modify line 5 below (the one starting with `for i in`) so that the
program prints the numbers `2,4,6,..,20` twice.

```rust
fn main() {
    let nums: Vec<u32> = (1..11).collect();

    for _ in 1..3 {
        for i in nums.map(unimplemented!()) {
            println!("{}", i);
        }
    }
}
```

## GUIs and callbacks

What better way to tie this all off than by writing a GUI and some
callbacks? I'm going to use GTK+ and the wonderful
[gtk-rs](http://gtk-rs.org/) set of crates. Our goal ultimately is to
create a GUI with a single button on it. When that button is clicked,
a message will be written to a file that says "I was clicked."

For this example, you'll definitely want to use a cargo project. Go
ahead and run:

```
$ cargo new clicky
$ cd clicky
```

Now add gtk as a dependency. Within the `[dependencies]` section of
the `Cargo.toml`, add the line:

```toml
gtk = "0.5"
```

And now we're going to rip off the sample code from gtk-rs's
website. Put this in your `main.rs` (bonus points if you type it
yourself instead of copy-pasting):

```rust
extern crate gtk;

use gtk::prelude::*;

use gtk::{Button, Window, WindowType};

fn main() {
    if gtk::init().is_err() {
        println!("Failed to initialize GTK.");
        return;
    }

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

    button.connect_clicked(|_| {
        println!("Clicked!");
    });

    gtk::main();
}
```

Assuming you've got all of your system libraries set up correctly,
running `cargo run` should get you a nice, simple GUI.

If you do have trouble installing the crates, check out [gtk-rs's
requirements page](http://gtk-rs.org/docs-src/requirements.html)
first.

## Replacing the callback

You may have noticed that sample code already includes a callback,
which prints `Clicked!` to stdout each time the button is
clicked. That certainly makes our life a little bit easier. Now,
inside of that callback, we need to:

* Open up a file
* Write some data to the file

We're going to take a first stab at this without doing any error
handling. Instead, we'll use `.unwrap()` on all of the `Result`
values, causing our program to `panic!` if something goes wrong. We'll
clean that up a bit later.

[Searching the standard library for
file](https://doc.rust-lang.org/std/fs/struct.File.html) quickly finds
[`std::fs::File`](https://doc.rust-lang.org/std/fs/struct.File.html),
which seems promising. It also seems like the `create` function will
be the easiest way to get started. We'll write to `mylog.txt`. The example at the top of that page shows `write_all` (thanks Rust for awesome API docs!). First, try out this bit of code:

```rust
let mut file = std::fs::File::create("mylog.txt");
file.write_all(b"I was clicked.\n");
```

After addressing exercise 6 below, you'll see this error message:

```
error[E0599]: no method named `write_all` found for type `std::fs::File` in the current scope
  --> src/main.rs:27:14
   |
27 |         file.write_all(b"I was clicked.\n");
   |              ^^^^^^^^^
   |
   = help: items from traits can only be used if the trait is in scope
help: the following trait is implemented but not in scope, perhaps add a `use` for it:
   |
3  | use std::io::Write;
```

Oh, that's something new. In order to use items from a trait, the
trait has to be in scope. Easy enough, we can just add `use
std::io::Write;` to our closure:

```rust
use std::io::Write;
let mut file = std::fs::File::create("mylog.txt");
file.write_all(b"I was clicked.\n");
```

__Exercise 6__ If you're following along with the code like you should
be, you probably got a different error message above, and the code
I've provided here doesn't actually fix everything. You need to add an
extra method call to convert a `Result<File, Error>` into a
`File`. Hint: I mentioned it above.

Go ahead and run this program (via `cargo run`), click the button a
few times, and close the window. Then look at the contents of
`mylog.txt`. No matter how many times you clicked, you'll only get one
line of output.

The problem is that each time the callback is called, we call `create`
from `File`, which overwrites the old file. One approach here would be
to create an appending file handle (awesome bonus exercise for anyone
who wants to take it on). We're going to take another approach.

## Share the file

Let's move our `create` call to _outside_ of the closure
definition. We'll open the file in the `main` function itself, the
closure can capture a mutable reference to the `file`, and all will be
well in the world.

Unfortunately, the compiler really dislikes this:

```
error[E0596]: cannot borrow `file` as mutable, as it is a captured variable in a `Fn` closure
  --> src/main.rs:28:9
   |
28 |         file.write_all(b"I was clicked.\n");
   |         ^^^^ cannot borrow as mutable
   |
help: consider changing this to accept closures that implement `FnMut`
  --> src/main.rs:26:28
   |
26 |       button.connect_clicked(|_| {
   |  ____________________________^
27 | |         use std::io::Write;
28 | |         file.write_all(b"I was clicked.\n");
29 | |     });
   | |_____^

error[E0597]: `file` does not live long enough
  --> src/main.rs:28:9
   |
26 |     button.connect_clicked(|_| {
   |                            --- value captured here
27 |         use std::io::Write;
28 |         file.write_all(b"I was clicked.\n");
   |         ^^^^ borrowed value does not live long enough
...
32 | }
   | - `file` dropped here while still borrowed
   |
   = note: borrowed value must be valid for the static lifetime...

error: aborting due to 2 previous errors

Some errors occurred: E0596, E0597.
For more information about an error, try `rustc --explain E0596`.
error: Could not compile `clicky`.
```

Or more briefly:

```
cannot borrow `file` as mutable, as it is a captured variable in a `Fn` closure
    help: consider changing this to accept closures that implement `FnMut`
`file` does not live long enough
    note: borrowed value must be valid for the static lifetime...
```

We can understand both of these by looking at the signature for [`connect_clicked`](http://gtk-rs.org/docs/gtk/trait.ButtonExt.html#tymethod.connect_clicked):

```rust
fn connect_clicked<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId
```

`connect_clicked` is a method which takes some function `f` of type
`F` and returns a `SignalHandlerId`. We're not using that return
value, so just ignore it. The function is a `Fn`. Therefore, we're
_not_ allowed to pass in a `FnMut` or an `FnOnce`. GTK must be allowed
to call that function multiple times without the restrictions of a
mutable context. So keeping a mutable reference won't work.

The other interesting thing is `+ 'static`. We briefly mentioned
lifetime parameters above. `'static` is a special lifetime parameter,
which means "can live for the entire lifetime of the program." As one
nice example of this, all string literals have type `&'static str`,
though we usually just write `&str`.

The problem is that our `file` does _not_ have `'static` lifetime. It
is created in the `main` function, remains in the `main` function, and
only lives as long as the `main` function. You may argue that the
`main` function lives the entire length of the program, but that's not
exactly true. In our example above, `button` will outlive `file` when
calling `drop`s (since `drop`s are performed in FILO order). If the
`drop` for a button decided to call the click
callback one more time, we'd have memory unsafety.

So what we're left with is: we need a closure which does not have a
mutable reference to local data. How do we do that?

## Move it

We can get the compiler to stop complaining about the lifetime by
moving the variable into the closure. Now we're guaranteed that the
`file` will live as long as the closure itself, meeting the guarantees
demanded by `'static`. Do accomplish this, stick `move` in front of
the closure.

This still doesn't solve our `Fn` issue, however. How can we allow our
callback to be called multiple times after moving the value in?

## Reference counting (hint: nope)

We've reached a point where the normal borrow rules of Rust simply
aren't enough. We cannot prove to the compiler that our callback will
obey the mutable reference rules: exactly one mutable reference will
exist at a given time. These kinds of situations occur often enough
that the standard library provides built in support for reference
counted types.

Add the following statement to the top of your `main.rs`:

```rust
use std::rc::Rc;
```

An `Rc` is a single threaded reference counted value. There's also an
`Arc` type, which is atomic, and can be used in multithreaded
applications. Since GTK is a single-threaded library, we're safe using
an `Rc` instead of an `Arc`. One really awesome thing about Rust is
that if you make a mistake about this, the compiler can catch
you. This is because `Rc` does not implement the `Sync` and `Send`
traits. See more in the
[`Send`](https://doc.rust-lang.org/std/marker/trait.Send.html)
documentation.

Anyway, back to our example. We can wrap up our original `file` with
reference counting with this:

```rust
let file = std::fs::File::create("mylog.txt").unwrap();
let file = Rc::new(file);
```

How do we then get access to the underlying `File` to use it? Turns
out: we don't need to do anything special. Keeping our original
`file.write_all` does what we want. This is because `Rc` implements
the `Deref` trait:

```rust
impl<T> Deref for Rc<T> {
    type Target = T;
    ...
}
```

This means that you can get a reference to a `T` from a `Rc<T>`. Since
method call syntax automatically takes a reference, everything
works. Nice.

Well, almost everything:

```
error[E0596]: cannot borrow data in a `&` reference as mutable
  --> src/main.rs:32:9
   |
32 |         file.write_all(b"I was clicked.\n");
   |         ^^^^ cannot borrow as mutable
```

Reference counting allows us to have multiple references to a value,
but they're all _immutable_ references. Looks like we haven't actually
made our situation any better than before, where we had ensured that
the single owner of our data was the closure.

## RefCell

`RefCell` is designed to exactly solve this problem. I'm not going to
go into detail explaining it, because the [API docs for `std::cell` do
that better than I
could](https://doc.rust-lang.org/std/cell/index.html). I recommend you
go read that intro now, come back and work on this code, and then go
read the docs again. Personally, I had to read that explanation about
4 or 5 times and bash my head against some broken code before it
finally sank in correctly.

Anyway, add `use std::cell::RefCell;`, and then wrap a `RefCell`
around the original `File`:

```rust
let file = std::fs::File::create("mylog.txt").unwrap();
let file = RefCell::new(file);
```

Now our code will fail to compile with a different message:

```
error[E0599]: no method named `write_all` found for type `std::cell::RefCell<std::fs::File>` in the current scope
  --> src/main.rs:29:14
   |
29 |         file.write_all(b"I was clicked.\n").unwrap();
   |
```

Unlike `Rc`, with `RefCell` we cannot rely on the `Deref`
implementation to get us a `File`. Instead, we'll need to use a method
on `RefCell` to get a reference to the `File`:

```rust
file.borrow().write_all(b"I was clicked.\n");
```

But that doesn't quite work:

```
error[E0596]: cannot borrow data in a `&` reference as mutable
```

Fortunately, that fix is as easy as using `borrow_mut()` instead. And
now our program works, hurray!

__NOTE__ Often, reference counting (`Rc` or `Arc`) and cells (`Cell`,
`RefCell`, or `Mutex`) go hand in hand, which is why my first instinct
in writing this lesson was to use both an `Rc` and a
`RefCell`. However, in this case, it turns out that just the `RefCell`
is sufficient.

## Exercise 7

The error handling in this program is lackluster. There are three problems:

1. If `gtk::init()` fails, the exit code of our program is still `0`
   (indicating success).
2. If opening `mylog.txt` fails, we panic.
3. If writing to the file fails, we panic.

To fix this, have `main` return a value of type `Result<(),
Box<std::error::Error>>`. Most other errors can be automatically
coerced via `From::from` into `Box<std::error::Error>`. For problems
(1) and (2), use the standard error handling mechanisms we discussed
back in lesson 3. For problem (3), print an error message with
`eprintln!` when an error occurs.

## Fearless concurrency!

It's finally time to do some fearless concurrency. We're going to
write a program which will:

* Allocate a string containing the word "Fearless"
* Fork a thread every second for 10 iterations
* In the forked thread:
    * Add another exclamation point to the string
    * Print the string

Before we begin, you can probably identify some complex pieces of
ownership that are going to go on here:

* Multiple threads will have access to some mutable data
* We need to ensure only one writer at a time
* We need to ensure that the data is released when everyone is done
  with it

Instead of trying to design a great solution to this from the
beginning, we'll treat this like a proper crash course. We'll do the
most naive stuff possible, look at the error messages, and try to
improve. If you think you can implement the complete program yourself
now, definitely give it a shot! Even if you don't think you can
implement it yourself, it's worth trying. The effort will make the
explanation below more helpful.

### Introducing the functions

We're going to use the following three functions:

[`std::thread::spawn`](https://doc.rust-lang.org/std/thread/fn.spawn.html)
to spawn a thread. It has an interesting signature:

```rust
pub fn spawn<F, T>(f: F) -> JoinHandle<T> where
    F: FnOnce() -> T,
    F: Send + 'static,
    T: Send + 'static,
```

The `Send` trait means that both the provided function and its return
value must be values which can be sent to a different thread. The
`'static` bit says that we cannot retain any references to local
variables. And the `FnOnce()` bit says that _any_ closure will work.

[`std::thread::sleep`](https://doc.rust-lang.org/std/thread/fn.sleep.html)
to have the main thread sleep. It takes a value of type `Duration`,
which brings us to our last function:

[`std::time::Duration::new`](https://doc.rust-lang.org/std/time/struct.Duration.html)
takes the number of seconds and nanoseconds in a duration.

Before we introduce the great fun which is spawning a new thread,
let's try a single threaded version:

```rust
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let mut msg: String = String::from("Fearless");
    for _ in 1..11 {
        msg.push('!');
        println!("{}", msg);
        sleep(Duration::new(1, 0));
    }
}
```

We can even wrap up that `msg.push` and `println!` in a closure to get
a bit closer to the call to `spawn`:

```rust
use std::thread::sleep;
use std::time::Duration;

fn main() {
    let mut msg: String = String::from("Fearless");
    for _ in 1..11 {
        let inner = || {
            msg.push('!');
            println!("{}", msg);
        };
        inner();
        sleep(Duration::new(1, 0));
    }
}
```

That gives us an error message:

```
error[E0596]: cannot borrow immutable local variable `inner` as mutable
```

Go ahead and fix that and make this compile.

### Introducing spawn

The simplest way to introduce spawn is to replace the `inner()` call
with `spawn(inner)`. Replace:

```rust
use std::thread::sleep;
```

with

```rust
use std::thread::{sleep, spawn};
```

And add the `spawn` call. We get the error message:

```
error[E0373]: closure may outlive the current function, but it borrows `msg`, which is owned by the current function
 --> main.rs:7:25
  |
7 |         let mut inner = || {
  |                         ^^ may outlive borrowed value `msg`
8 |             msg.push('!');
  |             --- `msg` is borrowed here
help: to force the closure to take ownership of `msg` (and any other referenced variables), use the `move` keyword
  |
7 |         let mut inner = move || {
  |                         ^^^^^^^

error: aborting due to previous error
```

Seems simple enough: we have to have a self contained closure to pass
to `spawn`, which can't refer to values from the parent thread. Let's
just add a `move` in front of the closure. We get an error message:

```
error[E0382]: capture of moved value: `msg`
 --> main.rs:8:13
  |
7 |         let mut inner = move || {
  |                         ------- value moved (into closure) here
8 |             msg.push('!');
  |             ^^^ value captured here after move
  |
  = note: move occurs because `msg` has type `std::string::String`, which does not implement the `Copy` trait
```

I still don't find these error messages particularly enlightening. But
it's telling us that we're trying to capture a moved value. This is
happening because we're moving the value into the closure in the first
iteration of the loop, and then trying to move it in again. That
clearly won't work!

### A broken solution

Let's just cheat and create a new copy of the string for each
iteration. That's easy enough: add the following above `let mut
inner`:

```rust
let mut msg = msg.clone();
```

This will compile (with a warning) and run, but it has the wrong
output. We aren't adding extra exclamation points each time. We're not
actually dealing with shared mutable data. Darn.

But that cloning gives me another idea...

### Reference counting

Maybe we can throw in that reference counting we mentioned previously,
and let each thread keep a pointer to the same piece of data.

```rust
use std::thread::{sleep, spawn};
use std::time::Duration;
use std::rc::Rc;

fn main() {
    let msg = Rc::new(String::from("Fearless"));
    for _ in 1..11 {
        let mut msg = msg.clone();
        let mut inner = move || {
            msg.push('!');
            println!("{}", msg);
        };
        spawn(inner);
        sleep(Duration::new(1, 0));
    }
}
```

Well, _that's_ a new one:

```
error[E0277]: `std::rc::Rc<std::string::String>` cannot be sent between threads safely
  --> main.rs:13:9
   |
13 |         spawn(inner);
   |         ^^^^^ `std::rc::Rc<std::string::String>` cannot be sent between threads safely
   |
```

There's that fearless concurrency we've heard so much about! The
compiler is preventing us from sending an `Rc` value between
threads. It would be nice if the compiler mentioned it, but we already
know that for multithreaded applications, we need an atomic reference
counter, or `std::sync::Arc`. Go ahead and switch over to that. You
should get a new error message:

```
error[E0596]: cannot borrow immutable borrowed content as mutable
  --> main.rs:10:13
   |
10 |             msg.push('!');
   |             ^^^ cannot borrow as mutable

error: aborting due to previous error
```

### Inner mutability

Above, I mentioned that `Rc` and `RefCell` usually go together. The
`Rc` provides reference counting, and the `RefCell` provides
mutability. Maybe we can combine `Arc` and `RefCell` too?

```rust
use std::thread::{sleep, spawn};
use std::time::Duration;
use std::sync::Arc;
use std::cell::RefCell;

fn main() {
    let msg = Arc::new(RefCell::new(String::from("Fearless")));
    for _ in 1..11 {
        let mut msg = msg.clone();
        let mut inner = move || {
            let msg = msg.borrow_mut();
            msg.push('!');
            println!("{}", msg);
        };
        spawn(inner);
        sleep(Duration::new(1, 0));
    }
}
```

More fearless concurrency:

```
error[E0277]: `std::cell::RefCell<std::string::String>` cannot be shared between threads safely
  --> main.rs:15:9
   |
15 |         spawn(inner);
   |         ^^^^^ `std::cell::RefCell<std::string::String>` cannot be shared between threads safely
   |
   = help: the trait `std::marker::Sync` is not implemented for `std::cell::RefCell<std::string::String>`
   = note: required because of the requirements on the impl of `std::marker::Send` for `std::sync::Arc<std::cell::RefCell<std::string::String>>`
   = note: required because it appears within the type `[closure@main.rs:10:25: 14:10 msg:std::sync::Arc<std::cell::RefCell<std::string::String>>]`
   = note: required by `std::thread::spawn`
```

You could go search for more info, but the normal way to have a
mutable, multithreaded cell is a `Mutex`. Instead of `borrow_mut()`,
we have a `lock()` method, which ensures that only one thread at a
time is using the mutex. Let's try that out:

```rust
use std::thread::{sleep, spawn};
use std::time::Duration;
use std::sync::{Arc, Mutex};

fn main() {
    let msg = Arc::new(Mutex::new(String::from("Fearless")));
    for _ in 1..11 {
        let mut msg = msg.clone();
        let mut inner = move || {
            let msg = msg.lock();
            msg.push('!');
            println!("{}", msg);
        };
        spawn(inner);
        sleep(Duration::new(1, 0));
    }
}
```

We get the error:

```
error[E0599]: no method named `push` found for type `std::result::Result<std::sync::MutexGuard<'_, std::string::String>, std::sync::PoisonError<std::sync::MutexGuard<'_, std::string::String>>>` in the current scope
```

Oh, right. `lock`ing can fail, due to something called
poisoning. ([Check out the
docs](https://doc.rust-lang.org/std/sync/struct.Mutex.html#poisoning)
for more information.) To quote the docs:

> Most usage of a mutex will simply `unwrap()` these results,
> propagating panics among threads to ensure that a possibly invalid
> invariant is not witnessed.

This is the closest to runtime exceptions I've seen the Rust docs
mention, nice. If we add that `.unwrap()`, we get told that `msg`
needs to be mutable. And if we add `mut`, we've written our first
multithreaded Rust application using shared mutable state.

Notice how the compiler prevented us from making some serious
concurrency mistakes? That's pretty awesome.

As a final step, see which `mut`s and `move`s you can and cannot
remove from the final program. Make sure you can justify to yourself
why the compiler does or does not accept each change.

## Next time

You're now deep into the hard parts of Rust. What comes now is getting
more comfortable with the hairy bits of ownership and closures, and to
get more comfortable with the library ecosystem. We're ready to get
much more real world next time, and learn about tokio, the de facto
standard async I/O framework in Rust.

[Rust at FP Complete](https://www.fpcomplete.com/rust) | [Introduction](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course)
