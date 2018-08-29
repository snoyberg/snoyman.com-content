Eliezer asked me a few questions about programming yesterday, and I
ended up demonstrating a little bit about pattern matching. He then
had a lot of fun showing a friend of his how programming works. I may
end up giving some lessons to some of my kids' friends in the next few
weeks, which should be interesting.

## Kids IDE

I've started using a minimalistic "kids IDE" I threw together for
teaching the kids. It's helped us not worry about details like
filepaths, saving, and running in a terminal. It's not a great tool,
but good enough. I've included links to better tools in the README,
though this fits my needs for the moment.

https://github.com/snoyberg/kids-haskell-ide#readme

I've set up AppVeyor to upload Windows executables to S3:

https://s3.amazonaws.com/www.snoyman.com/kids-ide/bin/kids-ide.exe

You'll also need to [install Stack](https://haskell-lang.org/get-started).

## Pattern matching strings

This morning, Eliezer and Gavriella both had their next "official"
lesson. I started over with that pattern matching
demonstration. First, I showed them this example:

```haskell
main = do
  sayNickname "Eliezer"
  sayNickname "Gavriella"
  sayNickname "Yakov"
  sayNickname "Lavi"

sayNickname realname =
  putStrLn (nickname realname)

nickname "Eliezer" = "Elie Belly"
nickname "Gavriella" = "Galla Galla"
nickname "Lavi" = "Fat baby"
nickname "Yakov" = "Koko"
```

They got the basic idea of this. (And I ended up introducing
`putStrLn` around here as well, which they were fine with.) However,
as I had them typing out some of this on their own, they ended up with
a lot of trouble around capitalization, which was a good introduction
to Haskell's rules around that. We'll see how in a bit.

## Pattern matching ints

After establishing that we could pattern match on strings, I switched
the code to this:

```haskell
main = do
  sayNickname 1
  sayNickname 2
  sayNickname 3
  sayNickname 4

sayNickname realname =
  putStrLn (nickname realname)

nickname 1 = "Elie Belly"
nickname "Gavriella" = "Galla Galla"
nickname "Lavi" = "Fat baby"
nickname "Yakov" = "Koko"
```

And gave them the challenge to rewrite `nickname` so that the code
worked, which wasn't too much of a problem. The misordering of `Lavi`
and `Yakov` between `main` and `nickname` did cause some confusion,
and then helped them understand better how pattern matching works. (I
didn't intentionally put that in, it somehow slipped in while the kids
were working on rewriting things.)

## Type signatures

I asked them what the type of `nickname` was, and they said function
(yay!). And then explained to them that you can tell Haskell
explicitly what a thing's type is, and typed this in:

```haskell
nickname :: ? -> ?
```

The funny syntax didn't give them too much trouble, and then we got to
fill in the question marks. I asked them what the output of the
function was, pointing at the string. I was surprised: they said the
type was "nickname" or "name." They accepted that it was a string, but
they didn't like that. (New theory: humans' brains are naturally
strongly typed.)

I then asked what the input was, and they said "number." I hadn't
taught them about `Int` yet, and didn't know about integers from math,
so I told them that integer is a kind of number, and that in Haskell
we call it `Int`. Filling in the type signature was fine.

I pointed out that some things (like `Int` and `String`) were upper
case, and some were lower case. I pointed out that concrete things
that "actually exist" (maybe not the best terminology) are
capitalized. We know what an `Int` is, for example. Variables are
things we don't know yet, and those are lowercase. And finally, you
can put whatever you want inside a string, but it has to match
exactly. That seemed to click fairly well for them.

## Enums

I pointed out that referring to the kids as numbers isn't good. (He
responded that I actually _do_ call them 1, 2, 4, and 8 sometimes...)
Anyway, I said that a better type for the `nickname` function would be
to take a `Child` as input. He said "can we say `Child = Int`," which
was a great insight. I showed up that, yes, we can do `type Child =
Int`, but that there's a better way.

I introduced the idea that `data` defines a new datatype, and then
showed them:

```haskell
data Child
  = Eliezer
  | Gavriella
  | Yakov
  | Lavi
```

Gavriella asked "what are those lines?" and I explained they mean
"or." Therefore, a child is Eliezer, or Gavriella, or Yakov, or
Lavi. They got this.

Exercise: fix the following program, uncommenting the lines in `main`.

```haskell
main = do
  sayNickname Eliezer
  --sayNickname Gavriella
  --sayNickname Yakov
  --sayNickname Lavi

sayNickname realname =
  putStrLn (nickname realname)

data Child
  = Eliezer
  | Gavriella
  | Yakov
  | Lavi

nickname :: Child -> String
nickname Eliezer = "Elie Belly"
--nickname 2 = "Galla Galla"
--nickname 3 = "Fat baby"
--nickname 4 = "Koko"
```

Some caveats where they got stuck:

* Using lower case `lavi` instead of `Lavi`. I got to explain how
  pattern matching a variable works, and "wildcards." They got this,
  though still needed to be coached on it.
* I'd been on the fence about including syntax highlighting in the
  kids IDE, but it turns out that the different colors of
  `Eliezer` and `lavi` helped Gavriella realize her mistake. Syntax
  highlighting is a _good thing_ here.
* I did ultimately have to give her a hint: "Lavi is BIG but you made
  him small."

I started to try and define `data Person = Kid Child | Adult Parent`,
but realized quickly it was too complicated for now and aborted.

## Recursion

I did _not_ teach this well, the children did not get the concepts
correctly. However, they did understand the code as I wrote it.

```haskell
main = print total

total = addUp 10

addUp 10 = 10 + addUp 9
addUp 9 = 9 + addUp 8
addUp 8 = 8 + addUp 7
addUp 7 = 7 + addUp 6
addUp 6 = 6 + addUp 5
addUp 5 = 5 + addUp 4
addUp 4 = 4 + addUp 3
addUp 3 = 3 + addUp 2
addUp 2 = 2 + addUp 1
addUp 1 = 1 + addUp 0
addUp 0 = 0
```

They _really_ hated how repetitive typing that out was, which was
exactly the goal. It was easy to convince them that this was stupid. I
then changed `total` to `addUp 11` and it broke. They understood why,
and so we started working on something better.

```haskell
main = print total

total = addUp 10

addUp 0 = 0
addUp x = x + addUp (x - 1)
```

I stepped through the executable of this, complete with pattern
matching. Doing this a few times in the future is probably a good
idea.

Eliezer asked what happens if we remove the `addUp 0 = 0` case. We
discussed it, he said it wouldn't work, and said it would "keep
going." I told him it was called an infinite loop, and we got a stack
overflow. Good insight.

Gavriella asked how long it took me to learn this stuff. I told her 12
years; after all, I only started learning Haskell in my twenties. It
made them feel pretty good that they were learning this stuff earlier
than I did.

I gave them an exercise to implement `multTo` instead of `addUp`. They
didn't understand this, or recursion, and I had to help them through
the whole thing. Mea culpa completely.

Gavriella asked for another exercise:

```haskell
main = do
  sayAge Eliezer
  sayAge Gavriella
  sayAge Yakov
  sayAge Lavi

data Child = ...

sayAge child = print (age child)

age ? = ?
```

She typed something like:

```haskell
age ? = ?
Eliezer=10
```

I showed her that she needed:

```haskell
age Eliezer = 10
```

And then she got the rest just fine, though with a few capitalization
mistakes.
