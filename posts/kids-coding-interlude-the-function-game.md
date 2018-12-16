Since the kids have been back at school and we've been busy with work
and some home renovations, I unfortunately haven't had a chance to
continue much with the kids coding training. However, when discussing
the general topic of education at [Functional
Conf](https://functionalconf.com/), the topic of "the function game"
came up, and I wanted to share what we did. I found this a
great&mdash;and perhaps vital&mdash;pre-training for Haskell.

The function game is simple: I pretend to be a function, let the kids
give me input, and I give them output. They need to try to figure out
what the function is. Here's an example conversation:

Me: When you give me 1, I give you 2. When you give me 2, I give you 3.  
Them: What if I give you 5?  
Me: 6  
Them: 8  
Me: 9  
Them: You're just adding 1!

With each kid, I've _always_ started off with addition. I'll later get
into identity, but that one is (perhaps surprisingly) more confusing
for them. Multiplication by 2 is a good follow-up to addition. Then,
when they get comfortable with discovering a few of these, I'll throw in:

* `f x = x * 2 + 3`
* `f x = x` (identity)
* `f x = 5` (constant)

We'll typically play this game at the dinner table. At some point I'll
also actually _define_ a function for them, after they've already
experienced some success at guessing what I'm doing:

> It always gives the same output for the same input

The next curveball I introduce is different types:

Me: When you give me "apple", I give you 5  
Them: What?  
Me: Functions don't just work on numbers  
Them: OK...  
Me: When you give me "book", I give you 4  
Them: It's the number of letters!

After that, each time I tell them I have a new function, they'll ask
me the type of the input. As a Haskeller father, I couldn't be more
proud :).

I'll also teach them about functions of multiple inputs:

Me: When you give me 2 and 3, I give you 5  
Them: What?  
Me: Functions can take more than 1 input.

And after a few more examples, they figure out that I'm just doing
addition.

I _think_ I tried demonstrating partial function application by saying
"when you give me 2, I give you a new function you can play with." But
I don't remember if I actually did this, or if I just planned it. And
since I'm sitting in a hotel lobby waiting for a cab, I can't test out
the theory on them right now.

Anyway, I hope this proves useful for others trying to teach their
kids (or maybe non-kids!) either math or functional programming. If
you try it out, please let me know how it goes.
