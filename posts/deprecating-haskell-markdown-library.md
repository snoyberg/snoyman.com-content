Back in 2012, I was working on a closed-source project which required
a Markdown parsing library. At the time, the only option in the
Haskell ecosystem I was aware of was Pandoc, which wasn't an option
for the project in question due to licensing (GPL). As a result, I
ended up creating my own library called
[markdown](https://www.stackage.org/package/markdown) (inventive name,
I know). It also had the benefit of significantly less dependencies
than Pandoc, and therefore faster build times. Today, I'm considering
deprecating this library, for a number of reasons:

*   The license concern no longer affects me. But more to the point:
    there are other Haskell markdown libraries with liberal licenses
    and low dependency footprints. Some examples include:

    * [cmark](https://www.stackage.org/package/cmark)
    * [sundown](https://www.stackage.org/package/sundown) (using Github's C library)
    * [cheapskate](https://www.stackage.org/lts-11.13/package/cheapskate-0.1.1)

*   There are too many different flavors of markdown floating around
    today, and I'm contributing to the problem by having _yet another
    flavor_.

*   I'm not particularly happy with the flavor I created. I'm not in
    any way an expert at parsing, and did not have a deep
    understanding of the Markdown spec (or lack thereof) when I wrote
    this library. I'm personally more comfortable using, for example,
    Github flavored Markdown.

I'm not worried about maintenance burden in this case; this library
hasn't caused me much trouble. The deprecation would primarily be to
get a flavor of Markdown off the market, and encourage people to use
something more standard. I'm also open to alternative solutions here,
like using the `markdown` package namespace for a better
implementation.

My biggest concern with a deprecation or changing the library
significantly is that it may break existing projects that are relying
on this specific flavor of markdown. But for those cases, the current
version of the code will always be available. I may end up using it
myself for some sites where I don't want to go back and fix content.

The purpose of this blog post, then, is to find out if there are
people out there who have a strong reason for me to _not_ deprecate
this library, or would like to see something specific happen with the
package name on Hackage/Stackage. If so, please let me know.

And as a second, lesser goal, I'm interested in hearing about the
current state of Markdown libraries in Haskell. Have I missed some
very popular ones? Is there any kind of general consensus on which
library to use? I've been favoring `sundown` recently, since I've been
working on projects where I want identical HTML output between Github
or Gitlab's web interface and the code I'm writing. But Common Mark is
a much more exciting project on the horizon.
