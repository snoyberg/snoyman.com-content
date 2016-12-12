This isn't any deep thought, and I'm sure others have mentioned it
before. But I haven't seen it called out explicitly, so I thought it
was worth getting it down.

Recently I was working on a customer project which required a specific
feature
([generate a Docker image with some libraries precompiled into it](https://github.com/fpco/stack-docker-image-build)). I'll
probably blog more about the specific case later, and give credit at
that time to the company that gave permission for the code to be open
sourced.

It turns out this is a problem that various FP Complete engineers have
solved for customers (and internal purposes) a few times
already. Creating a single open-source tool that can be shared among
projects is a clear win, and plays to all the strengths of open source
software. (And in this case, the initial version was
[really simple to implement](https://github.com/fpco/stack-docker-image-build/blob/v0.1.0.0/app/Main.hs),
so it was almost a no brainer.)

Not long after I released that first version, I needed to update some
Docker image build code for a _different_ customer, who until now had
been using a custom solution. So I moved them over to the new tool,
added some features that they needed, and got to the goal of a working
Docker image quicker than expected. Yay code sharing! And now others
can take advantage of this work, and contribute patches that both
projects using it will be able to take advantage of.

However, these are all the standard benefits of open sourcing. In this
process, I rediscovered something I've seen happen multiple times:

__When you're forced to separate out a tool or library, you create a
more general solution, and make your code more maintainable in the
long run.__

When you write a "throw-away" tool or a "internal" library, odds are
you won't think very hard about an extensible interface. You may embed
assumptions into its design. And then the code will sit in a
closed-source codebase for months or years, likely without anyone
touching it in the interim. When it turns out one of your assumptions
was wrong, or the interface needs to be extended, it's often times
much harder than updating a general purpose tool or library.

That's not to say that everything that _can_ be generalized _should_
be generalized and open sourced. There are some thing which are so
specific to a project that it's unlikely that any other use case will
exist. Or that the cognitive overhead of figuring out a good interface
is simply not warranted.

But for those sweet-spot cases where the overhead of doing something
general isn't too high, you have the prerogative to open source, and
there's likely at least one other person or project in the world who
can use it, you'll often thank yourself in the future for having taken
out the time to open source it.
