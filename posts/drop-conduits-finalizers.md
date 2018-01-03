tl;dr: I'm thinking of dropping finalizers from Conduit. If you have
use cases that necessitate finalizers, __please share them with
me__. [Github issue for examples.](https://github.com/snoyberg/conduit/issues/343)

People who have followed the history of the Conduit library will
likely be aware of how passionately I've argued for the necessity of a
finalizer concept. For those unaware: when you have a Conduit pipeline
like the following:

```haskell
runConduitRes $ do
  sourceFile input .| takeCE 100 .| sinkFile output
  -- lots of other stuff
```

Without finalizers, the file `input` will remain open until we exit
the `runConduitRes` call (unless the file is less than 100 bytes). The
reason is that, in a pipeline, downstream is always in control. When
`takeCE` stops consuming from `sourceFile`, `sourceFile` will never be
called again. Finalizers allow `sourceFile` to tell downstream "hey,
if you don't call me back, please close up my file." And so they're a
Good Thing.

They're also a Complicated Thing. The internals of Conduit include a
lot of complexity around finalizers. As a user, you hardly ever see
it, which is by design. But if you start dealing with the guts of
Conduit, it's there. (Have a look at
[this commit](https://github.com/snoyberg/conduit/commit/e7f0cb77987a23c3d259e413efa33f45f7069f79)
to get an idea.)

They also complicate the story around associativity of Conduit
significantly. Or more to the point: without finalizers, we can get
properly behaving associative and identity laws\*. With finalizers, we
have to start making up claims about reordering of finalizers. Which
is almost always fine in practice, but clearly not a mathematical law.

\* Caveats: I've never written the proof of it completely. Also, it
relies on using the type paramter on the `Pipe` type to eliminate
leftovers, but leftovers are not a topic I'm raising right now.

None of this is new information; so why am I writing this blog post
now? The first is that I'm already working on a breaking change to
Conduit to standardize naming and eliminate some legacy type and
operator names. See
[the discussion](https://github.com/snoyberg/conduit/issues/283) and
[initial comparison](https://github.com/snoyberg/conduit/pull/338/files)
for more details. This naturally leads me to ask related questions.

More to the point: after having worked with Conduit for years, my
initial concerns about prompt finalization seem to have been
overzealous. While the code above _can_ happen, it doesn't happen that
often in practice, and even that level of resource overholding isn't
usually that bad. Regardless, if the situation really does call for
guaranteed promptness, we can still get it (a trick I'm fairly certain
I learned from Gabriel Gonzalez):

```haskell
runConduitRes $ do
  withSourceFileResource input $ \src ->
    src .| takeCE 100 .| sinkFile output
  -- lots of other stuff
```

It's not quite as elegant, but works well enough. And again: I'm hard
pressed to think of real life code I've written that would really
warrant this. __BIG REQUEST__ If you have examples of Conduit-using
code where finalizers are vital, please send me examples.

While on the one hand, making this change would be to admit I've
sucked up huge amounts of extra work in maintaining Conduit over the
years, I'd be very happy to cut the dead weight now, unless someone
out there wants to convince me otherwise. Feel free to discuss
wherever desired, but the main discussion I'll be sure to follow will
be
[conduit issue #343](https://github.com/snoyberg/conduit/issues/343).

There are some natural follow-on questions that come from this also,
which I'll likely broach in a later blog post. To give a taste and
hopefully encourage some thoughts from others:

* Should the `Pipe` types upstream finalizer concept disappear as
  well?
* Can we remove the `Leftover` data constructor from `Pipe`, making
  `Pipe` a full category, and then move the tracking of leftovers to
  `ConduitM` and its codensity approach?
