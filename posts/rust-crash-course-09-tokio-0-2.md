In the [previous lesson in the crash course](/blog/2019/12/rust-crash-course-08-down-dirty-future), we covered the new `async/.await` syntax stabilized in Rust 1.39, and the `Future` trait which lives underneath it. This information greatly supercedes the now-defunct lesson 7 from last year, which covered the older `Future` approach.

Now it's time to update the second half of lesson 7, and teach the hot-off-the-presses Tokio 0.2 release. For those not familiar with it, let me quote the project's overview:

> Tokio is an event-driven, non-blocking I/O platform for writing asynchronous applications with the Rust programming language.

If you want to write an efficient, concurrent network service in Rust, you'll want to use something like Tokio. That's not to say that this is the only use case for Tokio; you can do lots of great things with an event driven scheduler outside of network services. It's also not to say that Tokio is the only solution; the [`async-std`](https://async.rs/) library provides similar functionality.

However, network services are likely the most common domain agitating for a non-blocking I/O system. And Tokio is the most popular and established of these systems today. So this combination is where we're going to get started.

And as a side note, if you have some other topic you'd like me to cover around this, please [let me know on Twitter](https://twitter.com/snoyberg).

*Exercise solutions will be included at the end of the blog post. Yes, I keep changing the rules, sue me.*

This post is part of a series based on [teaching Rust at FP
Complete](https://tech.fpcomplete.com/rust). If you're reading this post outside
of the blog, you can find links to all posts in the series [at the top of the
introduction
post](https://www.snoyman.com/blog/2018/10/introducing-rust-crash-course). You
can also [subscribe to the RSS
feed](https://www.snoyman.com/feed/rust-crash-course).

## Hello Tokio!

Let's kick this off. Go ahead and create a new Rust project for experimenting:

```shell
$ cargo new --bin usetokio
```

If you want to make sure you're using the same compiler version as me, set up your `rust-toolchain` correctly:

```shell
$ echo 1.39.0 > rust-toolchain
```

And then set up Tokio as a dependency. For simplicity, we'll install all the bells and whistles. In your `Cargo.toml`:

```toml
[dependencies]
tokio = { version = "0.2", features = ["full"] }
```

**PROTIP** You can run `cargo build` now to kick off the download and build of crates while you keep reading...

And now we're going to write an asynchronous hello world application. Type this into your `src/main.rs`:

```rust
use tokio::io;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut stdout = io::stdout();
    let mut hello: &[u8] = b"Hello, world!\n";
    io::copy(&mut hello, &mut stdout).await?;
    Ok(())
}
```

**NOTE** I specifically said "type this in" instead of "copy and paste." For getting comfortable with this stuff, I recommend manually typing in the code.

A lot of this should look familiar from our previous lesson. To recap:

* Since we'll be `await`ing something and generating a `Future`, our `main` function is `async`.
* Since `main` is `async`, we need to use an executor to run it. That's why we use the `#[tokio::main]` attribute.
* Since performing I/O can fail, we return a `Result`.

The first really new thing since last lesson is this little bit of syntax:

```rust
.await?
```

I mentioned it last time, but now we're seeing it in real life. This is just the combination of our two pieces of prior art: `.await` for chaining together `Future`s, and `?` for error handling. The fact that these work together so nicely is really awesome. I'll probably mention this a few more times, because I love it that much.

The next thing to note is that we use `tokio::io::stdout()` to get access to some value that lets us interact with standard output. If you're familiar with it, this looks really similar to `std::io::stdout()`. That's by design: a large part of the `tokio` API is simply async-ifying things from `std`.

And finally, we can look at the actual `tokio::io::copy` call. As you may have guessed, and as stated in the [API docs](https://docs.rs/tokio/0.2.2/tokio/io/fn.copy.html):

> This is an asynchronous version of [`std::io::copy`](https://doc.rust-lang.org/std/io/fn.copy.html).

However, instead of working with the `Read` and `Write` traits, this works with their async cousins: `AsyncRead` and `AsyncWrite`. A byte slice (`&[u8]`) is a valid `AsyncRead`, so we're able to store our input there. And as you may have guessed, `Stdout` is an `AsyncWrite`.

__EXERCISE 1__ Modify this application so that instead of printing "Hello, world!", it copies the entire contents of standard input to standard output.

__NOTE__ You can simplify this code using `stdout.write_all` after `use`ing `tokio::io::AsyncWriteExt`, but we'll stick to `tokio::io::copy`, since we'll be using it throughout. But if you're curious:

```rust
use tokio::io::{self, AsyncWriteExt};

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut stdout = io::stdout();
    stdout.write_all(b"Hello, world!\n").await?;
    Ok(())
}
```

## Spawning processes

Tokio provides a `tokio::process` module which resembles the `std::process` module. We can use this to implement Hello World once again:

```rust
use tokio::process::Command;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    Command::new("echo").arg("Hello, world!").spawn()?.await?;
    Ok(())
}
```

Notice how the `?` and `.await` bits can go in whatever order they are needed. You can read this line as:

* Create a new `Command` to run `echo`
* Give it the argument `"Hello, world!"`
* Spawn this, which may fail
* Using the first `?`: if it fails, return the error. Otherwise, return a `Future`
* Using the `.await`: wait until that `Future` completes, and capture its `Result`
* Using the second `?`: if that `Result` is `Err`, return that error.

Pretty nice for a single line!

One of the great advantages of `async`/`.await` versus the previous way of doing async with callbacks is how easily it works with looping.

__EXERCISE 2__ Extend this example so that it prints `Hello, world!` 10 times.

## Take a break

So far we've only really done a single bit of `.await`ing. But it's easy enough to `.await` on multiple things. Let's use [`delay_for`](https://docs.rs/tokio/0.2.2/tokio/time/fn.delay_for.html) to pause for a bit.

```rust
use tokio::time;
use tokio::process::Command;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    Command::new("date").spawn()?.await?;
    time::delay_for(Duration::from_secs(1)).await;
    Command::new("date").spawn()?.await?;
    time::delay_for(Duration::from_secs(1)).await;
    Command::new("date").spawn()?.await?;
    Ok(())
}
```

We can also use the `tokio::time::interval` function to create a stream of "ticks" for each time a certain amount of time has passed. For example, this program will keep calling `date` once per second until it is killed:

```rust
use tokio::time;
use tokio::process::Command;
use std::time::Duration;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut interval = time::interval(Duration::from_secs(1));
    loop {
        interval.tick().await;
        Command::new("date").spawn()?.await?;
    }
}
```

__EXERCISE 3__ Why isn't there a `Ok(())` after the `loop`?

## Time to spawn

This is all well and good, but we're not really taking advantage of asynchronous programming at all. Let's fix that! We've seen two different interesting programs:

1. Infinitely pausing 1 seconds and calling `date`
2. Copying all input from `stdin` to `stdout`

It's time to introduce `spawn` so that we can combine these two into one program. First, let's demonstrate a trivial usage of `spawn`:

```rust
use std::time::Duration;
use tokio::process::Command;
use tokio::task;
use tokio::time;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    task::spawn(dating()).await??;
    Ok(())
}

async fn dating() -> Result<(), std::io::Error> {
    let mut interval = time::interval(Duration::from_secs(1));
    loop {
        interval.tick().await;
        Command::new("date").spawn()?.await?;
    }
}
```

You may be wondering: what's up with that `??` operator? Is that some special super-error handler? No, it's just the normal error handling `?` applied twice. Let's look at some type signatures to help us out here:

```rust
pub fn spawn<T>(task: T) -> JoinHandle<T::Output>;

impl<T> Future for JoinHandle<T> {
    type Output = Result<T, JoinError>;
}
```

Calling `spawn` gives us back a `JoinHandle<T::Output>`. In our case, the `Future` we provide as input is `dating()`, which has an output of type `Result<(), std::io::Error>`. So that means the type of `task::spawn(dating())` is `JoinHandle<Result<(), std::io::Error>>`.

We also see that `JoinHandle` implements `Future`. So when we apply `.await` to this value, we end up with whatever that `type Output = Result<T, JoinError>` thing is. Since we know that `T` is `Result<(), std::io::Error>`, this means we end up with `Result<Result<(), std::io::Error>, JoinError>`.

The first `?` deals with the outer `Result`, exiting with the `JoinError` on an `Err`, and giving us a `Result<(), std::io::Error>` value on `Ok`. The second `?` deals with the `std::io::Error`, giving us a `()` on `Ok`. Whew!

__EXERCISE 4__ Now that we've seen `spawn`, you should modify the program so that it calls both `date` in a loop, and copies `stdin` to `stdout`.

## Synchronous code

You may not have the luxury of interacting exclusively with `async`-friendly code. Maybe you have some really nice library you want to leverage, but it performs blocking calls internally. Fortunately, Tokio's got you covered with the [`spawn_blocking`](https://docs.rs/tokio/0.2.2/tokio/task/fn.spawn_blocking.html) function. Since the docs are so perfect, let me quote them:

> The `task::spawn_blocking` function is similar to the `task::spawn` function discussed in the previous section, but rather than spawning an `non-blocking` future on the Tokio runtime, it instead spawns a `blocking` function on a dedicated thread pool for blocking tasks.

__EXERCISE 5__ Rewrite the `dating()` function to use `spawn_blocking` and `std::thread::sleep` so that it calls `date` approximately once per second.

## Let's network!

I could keep stepping through the other cools functions in the Tokio library. I encourage you to poke around at them yourself. But I promised some networking, and by golly, I'm gonna deliver!

I'm going to slightly extend the example from the [`TcpListener` docs](https://docs.rs/tokio/0.2.2/tokio/net/struct.TcpListener.html#examples) to (1) make it compile and (2) implement an echo server. This program has a pretty major flaw in it though, I recommend trying to find it.

```rust
use tokio::io;
use tokio::net::{TcpListener, TcpStream};

#[tokio::main]
async fn main() -> io::Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;

    loop {
        let (socket, _) = listener.accept().await?;
        echo(socket).await?;
    }
}

async fn echo(socket: TcpStream) -> io::Result<()> {
    let (mut recv, mut send) = io::split(socket);
    io::copy(&mut recv, &mut send).await?;
    Ok(())
}
```

We use `TcpListener` to bind a socket. The binding itself is asynchronous, so we use `.await` to wait for the listening socket to be available. And we use `?` to deal with any errors while binding the listening socket.

Next, we loop forever. Inside the loop, we accept new connections, using `.await?` like before. We capture the `socket` (ignoring the address as the second part of the tuple). Then we call our `echo` function and `.await` it.

Within `echo`, we use `tokio::io::split` to split up our `TcpStream` into its constituent read and write halves, and then pass those into `tokio::io::copy`, as we've done before.

Awesome! Where's the bug? Let me ask you a question: what _should_ the behavior be if a second connection comes in while the first connection is still active? Ideally, it would be handled. However, our program has just one task. And that task `.await`s on each call to `echo`. So our second connection won't be serviced until the first one closes.

__EXERCISE 6__ Modify the program above so that it handles concurrent connections correctly.

## TCP client and ownership

Let's write a poor man's HTTP client. It will establish a connection to a hard-coded server, copy all of `stdin` to the server, and then copy all data from the server to `stdout`. To use this, you'll manually type in the HTTP request and then hit `Ctrl-D` for end-of-file.

```rust
use tokio::io;
use tokio::net::TcpStream;

#[tokio::main]
async fn main() -> io::Result<()> {
    let stream = TcpStream::connect("127.0.0.1:8080").await?;
    let (mut recv, mut send) = io::split(stream);
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    io::copy(&mut stdin, &mut send).await?;
    io::copy(&mut recv, &mut stdout).await?;

    Ok(())
}
```

That's all well and good, but it's limited. It only handles half-duplex protocols like HTTP, and doesn't actually support keep-alive in any way. We'd like to use `spawn` to run the two `copy`s in different tasks. Seems easy enough:

```rust
let send = spawn(io::copy(&mut stdin, &mut send));
let recv = spawn(io::copy(&mut recv, &mut stdout));

send.await??;
recv.await??;
```

Unfortunately, this doesn't compile. We get four nearly-identical error messages. Let's look at the first:

```
error[E0597]: `stdin` does not live long enough
  --> src/main.rs:12:31
   |
12 |     let send = spawn(io::copy(&mut stdin, &mut send));
   |                      ---------^^^^^^^^^^------------
   |                      |        |
   |                      |        borrowed value does not live long enough
   |                      argument requires that `stdin` is borrowed for `'static`
...
19 | }
   | - `stdin` dropped here while still borrowed
```

Here's the issue: our `copy` `Future` does not _own_ the `stdin` value (or the `send` value, for that matter). Instead, it has a (mutable) reference to it. That value remains in the `main` function's `Future`. Ignoring error cases, we know that the `main` function will wait for `send` to complete (thanks to `send.await`), and therefore the lifetimes appear to be correct. However, Rust doesn't recognize this lifetime information. (Also, and I haven't thought this through completely, I'm fairly certain that `send` may be dropped earlier than the `Future` using it in the case of `panic`s.)

In order to fix this, we need to convince the compiler to make a `Future` that owns `stdin`. And the easiest way to do that here is to use an `async move` block.

__Exercise 7__ Make the code above compile using two `async move` blocks.

## Playing with `lines`

This section will have a series of modifications to a program. I recommend you solve each challenge before looking at the solution. However, unlike the other exercises, I'm going to show the solutions inline since they build on each other.

Let's build an async program that counts the number of lines on standard input. You'll want to use the [`lines`](https://docs.rs/tokio/0.2.2/tokio/io/trait.AsyncBufReadExt.html#method.lines) method for this. Read the docs and try to figure out what `use`s and wrappers will be necessary to make the types line up.

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let stdin = io::stdin();
    let stdin = io::BufReader::new(stdin);
    let mut count = 0u32;
    let mut lines = stdin.lines();
    while let Some(_) = lines.next_line().await? {
        count += 1;
    }
    println!("Lines on stdin: {}", count);
    Ok(())
}
```

OK, bumping this up one more level. Instead of standard input, let's take a list of file names as command line arguments, and count up the total number of lines in all the files. Initially, it's OK to read the files one at a time. In other words: don't bother calling `spawn`. Give it a shot, and then come back here:

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut count = 0u32;

    for filename in args {
        let file = tokio::fs::File::open(filename).await?;
        let file = io::BufReader::new(file);
        let mut lines = file.lines();
        while let Some(_) = lines.next_line().await? {
            count += 1;
        }
    }

    println!("Total lines: {}", count);
    Ok(())
}
```

But now it's time to make this properly asynchronous, and process the files in separate `spawn`ed tasks. In order to make this work, we need to spawn all of the tasks, and then `.await` each of them. I used a `Vec` of `Future<Output=Result<u32, std::io::Error>>`s for this. Give it a shot!

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];

    for filename in args {
        tasks.push(tokio::spawn(async {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut count = 0u32;
            while let Some(_) = lines.next_line().await? {
                count += 1;
            }
            Ok(count) as Result<u32, std::io::Error>
        }));
    }

    let mut count = 0;
    for task in tasks {
        count += task.await??;
    }

    println!("Total lines: {}", count);
    Ok(())
}
```

And finally in this progression: let's change how we handle the `count`. Instead of `.await`ing the count in the second `for` loop, let's have each individual task update a shared mutable variable. You should use an `Arc<Mutex<u32>>` for that. You'll still need to keep a `Vec` of the tasks though to ensure you wait for all files to be read.

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;
use std::sync::Arc;

// avoid thread blocking by using Tokio's mutex
use tokio::sync::Mutex;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];
    let count = Arc::new(Mutex::new(0u32));

    for filename in args {
        let count = count.clone();
        tasks.push(tokio::spawn(async move {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut local_count = 0u32;
            while let Some(_) = lines.next_line().await? {
                local_count += 1;
            }

            let mut count = count.lock().await;
            *count += local_count;
            Ok(()) as Result<(), std::io::Error>
        }));
    }

    for task in tasks {
        task.await??;
    }

    let count = count.lock().await;
    println!("Total lines: {}", *count);
    Ok(())
}
```

## LocalSet and `!Send`

Thanks to [@xudehseng](https://twitter.com/xudesheng/status/1201382514415325185?s=20) for the inspiration on this section.

OK, did that last exercise seem a bit contrived? It was! In my opinion, the previous approach of `.await`ing the counts and summing in the `main` function itself was superior. However, I wanted to teach you something else.

What happens if you replace the `Arc<Mutex<u32>>` with a `Rc<RefCell<u32>>`? With this code:

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;
use std::rc::Rc;
use std::cell::RefCell;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];
    let count = Rc::new(RefCell::new(0u32));

    for filename in args {
        let count = count.clone();
        tasks.push(tokio::spawn(async {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut local_count = 0u32;
            while let Some(_) = lines.next_line().await? {
                local_count += 1;
            }

            *count.borrow_mut() += local_count;
            Ok(()) as Result<(), std::io::Error>
        }));
    }

    for task in tasks {
        task.await??;
    }

    println!("Total lines: {}", count.borrow());
    Ok(())
}
```

You get an error:

```
error[E0277]: `std::rc::Rc<std::cell::RefCell<u32>>` cannot be shared between threads safely
  --> src/main.rs:15:20
   |
15 |         tasks.push(tokio::spawn(async {
   |                    ^^^^^^^^^^^^ `std::rc::Rc<std::cell::RefCell<u32>>` cannot be shared between threads safely
   |
  ::: /Users/michael/.cargo/registry/src/github.com-1ecc6299db9ec823/tokio-0.2.2/src/task/spawn.rs:49:17
   |
49 |     T: Future + Send + 'static,
   |                 ---- required by this bound in `tokio::task::spawn::spawn`
```

Tasks can be scheduled to multiple different threads. Therefore, your `Future` must be `Send`. And `Rc<RefCell<u32>>` is definitely `!Send`. However, in our use case, using multiple OS threads is unlikely to speed up our program; we're going to be doing lots of blocking I/O. It would be nice if we could insist on spawning all our tasks on the same OS thread and avoid the need for `Send`. And sure enough, Tokio provides such a function: `tokio::task::spawn_local`. Using it (and adding back in `async move` instead of `async`), our program compiles, but breaks at runtime:

```
thread 'main' panicked at '`spawn_local` called from outside of a local::LocalSet!', src/libcore/option.rs:1190:5
```

Uh-oh! Now I'm personally not a big fan of this detect-it-at-runtime stuff, but the concept is simple enough: if you want to spawn onto the current thread, you need to set up your runtime to support that. And the way we do that is with [`LocalSet`](https://docs.rs/tokio/0.2.2/tokio/task/struct.LocalSet.html). In order to use this, you'll need to ditch the `#[tokio::main]` attribute.

__EXERCISE 8__ Follow the documentation for `LocalSet` to make the program above work with `Rc<RefCell<u32>>`.

## Conclusion

That lesson felt short. Definitely compared to the previous Tokio lesson which seemed to go on forever. I think this is a testament to how easy to use the new `async/`.await` syntax is.

There's obviously a lot more that can be covered in asynchronous programming, but hopefully this establishes the largest foundations you need to understand to work with the `async`/`.await` syntax and the Tokio library itself.

If we have future lessons, I believe they'll cover additional libraries like Hyper as they move over to Tokio 0.2, as well as specific use cases people raise. If you want something covered, mention it to me on Twitter or in the comments below.

## Solutions

### Solution 1

```rust
use tokio::io;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    io::copy(&mut stdin, &mut stdout).await?;
    Ok(())
}
```

### Solution 2

```rust
use tokio::process::Command;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    for _ in 1..=10 {
        Command::new("echo").arg("Hello, world!").spawn()?.await?;
    }
    Ok(())
}
```

### Solution 3

Since the `loop` will either run forever or be short circuited by an error, any code following `loop` will never actually be called. Therefore, code placed there will generate a warning.

### Solution 4

```rust
use std::time::Duration;
use tokio::process::Command;
use tokio::{io, task, time};

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let dating = task::spawn(dating());
    let copying = task::spawn(copying());

    dating.await??;
    copying.await??;

    Ok(())
}

async fn dating() -> Result<(), std::io::Error> {
    let mut interval = time::interval(Duration::from_secs(1));
    loop {
        interval.tick().await;
        Command::new("date").spawn()?.await?;
    }
}

async fn copying() -> Result<(), std::io::Error> {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    io::copy(&mut stdin, &mut stdout).await?;
    Ok(())
}
```

### Solution 5

```rust
async fn dating() -> Result<(), std::io::Error> {
    loop {
        task::spawn_blocking(|| { std::thread::sleep(Duration::from_secs(1)) }).await?;
        Command::new("date").spawn()?.await?;
    }
}
```

### Solution 6

The simplest tweak is to wrap the `echo` call with `tokio::spawn`:

```rust
loop {
    let (socket, _) = listener.accept().await?;
    tokio::spawn(echo(socket));
}
```

There is a downside to this worth noting, however: we're ignoring the errors produced by the spawned tasks. Likely the best behavior in this case is to handle the errors inside the spawned task:

```rust
#[tokio::main]
async fn main() -> io::Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:8080").await?;

    let mut counter = 1u32;
    loop {
        let (socket, _) = listener.accept().await?;
        println!("Accepted connection #{}", counter);
        tokio::spawn(async move {
            match echo(socket).await {
                Ok(()) => println!("Connection #{} completed successfully", counter),
                Err(e) => println!("Connection #{} errored: {:?}", counter, e),
            }
        });
        counter += 1;
    }
}
```

### Exericse 7

```rust
use tokio::io;
use tokio::spawn;
use tokio::net::TcpStream;

#[tokio::main]
async fn main() -> io::Result<()> {
    let stream = TcpStream::connect("127.0.0.1:8080").await?;
    let (mut recv, mut send) = io::split(stream);
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    let send = spawn(async move {
        io::copy(&mut stdin, &mut send).await
    });
    let recv = spawn(async move {
        io::copy(&mut recv, &mut stdout).await
    });

    send.await??;
    recv.await??;

    Ok(())
}
```

## Solution 8

```rust
use tokio::prelude::*;
use tokio::io::AsyncBufReadExt;
use std::rc::Rc;
use std::cell::RefCell;

fn main() -> Result<(), std::io::Error> {
    let mut rt = tokio::runtime::Runtime::new()?;
    let local = tokio::task::LocalSet::new();
    local.block_on(&mut rt, main_inner())
}

async fn main_inner() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let _me = args.next(); // ignore command name
    let mut tasks = vec![];
    let count = Rc::new(RefCell::new(0u32));

    for filename in args {
        let count = count.clone();
        tasks.push(tokio::task::spawn_local(async move {
            let file = tokio::fs::File::open(filename).await?;
            let file = io::BufReader::new(file);
            let mut lines = file.lines();
            let mut local_count = 0u32;
            while let Some(_) = lines.next_line().await? {
                local_count += 1;
            }

            *count.borrow_mut() += local_count;
            Ok(()) as Result<(), std::io::Error>
        }));
    }

    for task in tasks {
        task.await??;
    }

    println!("Total lines: {}", count.borrow());
    Ok(())
}
```
