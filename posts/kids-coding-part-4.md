Previous lessons have all been focused on teaching our ten and eight
year olds some coding, since our six year old (Yakov) is still working
on reading and writing in English. However, Yakov's been home sick all
week, and he asked me today to teach him some programming. So I came
up with a simplified version focused solely on the GHCi prompt.

I started off with:

```
> x = 32
> x + x
```

And then I asked him what he thought the answer would be. He quickly
came back with 64. Then I typed in:

```
> y = 5
> x + y
```

And he got 37 pretty quickly. We played around with a few more bits of
simple arithmetic (he hasn't really mastered multiplication yet). I
introduced defining variables in terms of other variables:

```
> x = 2 * 3
> y = x * 2
> y + 3
```

This took him a _bit_ longer, but entirely due to the multiplication!
This showed me a few things:

1. Reassigning variables was not confusing for him in the least
2. Variable replacement was similarly trivial

Finally, I decided to push things just a bit further and introduce
functions:

```
> f x = x + 3
> f 7
```

This confused him at first, but once I explained that this was
applying the function `f` to the number 7, he got it, and said "oh,
it's the +3 function." (Remember from [last
time](https://www.snoyman.com/blog/2018/12/kids-coding-interlude-the-function-game)
that he's been playing the function game for a while.) Next I hit:

```
> x = f 0
> f x
```

This was easy: it's 6! Finally I gave him two more challenging ones:

```
> f (f 0)
> f (f (f 10))
```

I fully expected confusion about parentheses. I was shocked: he wasn't
bothered by them at all. He immediately got both answers, and was very
proud of himself.

Total time: less than 10 minutes, probably closer to 5. Which is good,
because he's got a short attention span and wanted to play some
Nintendo with me too. Overall, I was _very_ happy with how many
concepts he was able to absorb.

(Thanks to my `~/.ghc/ghci_history` file for reminding me what we
covered today.)
