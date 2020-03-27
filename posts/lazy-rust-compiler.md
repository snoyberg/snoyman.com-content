Let’s face it: we all know that Rust is the best language on the market today, hands down. It has stolen all of the good features from all other languages out there, added some of its own, and is just cranking on awesomeness. The handwriting is on the wall: Rust wins, and all software in the next five years will be rewritten in Rust.

But there’s one prominent language feature that the Rust authors forgot about: laziness. Laziness is arguably the defining feature of Haskell, and despite all of Haskell’s many other limitations (strong typing, Software Transactional Memory, etc), laziness stands apart as a feature universally accepted as good.

It’s a true shame that the Rust language authors were so short sighted as to include unimportant features like enums and pattern matching in their language, yet leave out laziness. And so I’m happy to announce a new project: Lazy Rust, aka Lust.

Lust is going to be a source-compatible version of the Rust language. It has the capability to compile all existing Rust code without modification. However, it will automatically, transparently, and quickly perform laziness rewrites. This will give massive performance speedups in many common cases, be no less efficient in others, and introduce zero differences in runtime behavior otherwise.

I’ve written an extensive proof for these claims, but there’s no room in this already overly long blog post to include it.

The question of course is: how will we get Lust out into the world? And this is the cool thing: we get it for free by rewriting the Rust compiler in Haskell. You see, Haskell provides laziness using a really simple feature, called thunks. We could try to implement thunks directly in Rust today, but that’s a lot of work. Instead, rewriting a parser, type checker, code generator, and other tooling in Haskell is far easier.

There’s only one downside to this implementation strategy: all Lust programs will have a runtime dependency on Haskell’s premier compiler, GHC. However, this is a small price to pay for the massive benefits that laziness will bring in practice.
