I recently reinstalled the OS on my laptop and very quickly ran into:

```
$ warp
-bash: warp: command not found
```

This made me realize just how frequently I use the `warp` executable in my day-to-day life, and decided to write a quick post about it.

## How to install it

The executable may be available in your package manager. However, I recommend building/installing it with [Stack](https://docs.haskellstack.org):

1. [Install Stack](https://tech.fpcomplete.com/haskell/get-started), with `curl -sSL https://get.haskellstack.org/ | sh` or [the Windows installer](https://www.stackage.org/stack/windows-x86_64-installer)
2. Run `stack install wai-app-static`, or `stack install wai-app-static --resolver lts-14.20` to be a bit more pedantic
3. There is no step 3

If you don't have a Haskell toolchain set up, this will download a bunch of stuff and may take a bit. Sorry.

## What it does

The Warp executable provides a simple file server. My most usually way of calling it is without any arguments, where it will serve the files in the current directory on port 3000. The two most common options I'll pass it are:

```
$ warp -d some-directory # serve from that directory
$ warp -p 8080 # serve on a different port
```

You can use `warp --help` for a full listing of options, though there aren't many.

## Why it's useful

Simply: no config file. You can set up a dedicated file server using `nginx` or others, but that takes much more effort. Often times, I want to simply generate some HTML and then view it on my phone, for example. This works out perfectly.

Along those lines, I highly recommend checking out [ngrok](https://ngrok.com/), which will provide a temporary public URL to a locally running service. This can be a great way to share a locally running site with someone else. It's pretty common for me to have `warp` running in one terminal and `ngrok http 3000` in another.

## Also: Servius

I've considered writing a blog post about [Servius](https://www.stackage.org/package/servius) many times before, but there's never enough info to warrant a dedicated post. So now's a good time to mention it. Servius is a souped-up version of the Warp executable. In addition to serving static files, it has support for rendering Markdown (using Github-flavored CommonMark). I'll often use Servius instead of Warp when I'm drafting a blog post like this and want to check it out quickly.

Want to install Servius? Just run `stack install servius` or `stack install servius --resolver lts-14.20`.

## Other tools?

One more shout-out: I really like the [`bat` tool](https://github.com/sharkdp/bat#readme), aka "a cat clone with wings." To get it, [install Rust](https://www.rust-lang.org/tools/install) and run `cargo install bat`, or check out the [many other installation options](https://github.com/sharkdp/bat#installation). I enjoy getting syntax highlighting and paging when I want to look at files, without having to pop them open in `vim`.

I may expand this list over time :)
