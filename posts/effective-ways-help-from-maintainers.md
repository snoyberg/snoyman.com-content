This blog post was previously titled "Manipulating Maintainers," but
has been retitled to more accurately reflect what it's about (with a
less cheeky tone). It's about how to most effectively interact with
and request assistance from maintainers of open source projects, and
open source community members in general.

There's an old "ha ha, only serious" joke. If you go to a Linux forum
and ask for help fixing your WiFi driver, everyone will ignore
you. If, instead, you say "Linux sucks, you can't even get a f*&$ing
WiFi driver working!" thousands of people will solve the problem for
you.

This story is a great example of manipulating people, but it's
obviously a negative take on it. I'd like to share some thoughts on
this from a much more positive standpoint, which will help you get
people to pay more attention, be more helpful, and&mdash;perhaps most
importantly&mdash;create a healthier open source community over all.

These items will appear in no particular order, and will almost all
fall into either the *attractor* or *obstacle* category. An attractor
is something you can do to make people want to participate with
you. An obstacle is something you should not do, which would prevent
people from interacting with you.

And it should go without saying, but I'll say it anyway: this is an
opinionated list, written by one guy. I'm including in here things
that I personally care about, and things which friends and colleagues
have shared with me. No example is specific to any individual, so
don't think I'm calling you out: I'm most certainly _not_. And some
people may disagree, or have other items for this list. Sharing such
differing thoughts would be very healthy.

## Don't waste people's time

This is my biggest one to be honest. Remember that for the most part,
people you interact with in open source settings are doing this in
their free time. Either because they love a project, they want to help
people, they want to change the world, or something else. You're
asking for a slice of their lives. Make that slice as small as
possible.

The advice is vague, so let me follow up with some concrete examples:

* __File a good bug report.__ If you write an issue that says "my code
  doesn't compile," and don't include the error message, full code,
  OS, compiler version, and other such things, people will have to
  spend time prying it out of you. Be forthcoming with relevant
  information.
* In slight contradiction to that: __be concise__. Start off with the
  most highly pertinent information. Make it clear what you're trying
  to do. If you have a 400 line error message, perhaps put it in an
  lpaste or Gist and link to it.
* Provide a __minimal repro__. "Here's a link to my 1500 SLOC project
  that doesn't compile, kthxbye" is a bad start. As someone helping
  you, I'm going to have to strip off the extraneous bits until I'm
  staring the problem in the face. Why should I spend my time doing
  that, when you&mdash;the person asking for help&mdash;could be?
* Make sure to mention __any custom configuration__. If I spend 5 days
  trying to fix an issue with your code not linking, and then you say
  "oh, I've been trying out the prerelease linker, I forgot to mention
  that, you think that could be the problem?" I'm going to be
  annoyed. Don't do that. Be forthcoming with all relevant info, and
  call out in particular custom things on your system.
* Don't fall into __the XY problem__. And don't get offended if you
  get accused of hitting the XY problem. Instead of trying to explain
  what this problem is, I'll just
  [provide a link](http://xyproblem.info/).

## Demonstrate you've tried

You've been staring at your screen for the past 5 days. The code
hasn't compiled. You have no idea why. You're pulling out your hair at
this point (side note: bald is awesome). You finally, in a fit of
desperation, go to Stack Overflow and say:

> I'm trying to make an HTTP request in Haskell and can't figure it
> out. Any advice?

Good luck. Not only is your question vague, but it sounds like you're
asking for help on a homework assignment and are too lazy to do any
research. Make it clear that you're not just getting someone else to
do your work, and are really deserving of assistance.

> Below you'll find a snippet of code I've been trying to get working
> to make an HTTP PUT request and parse the response body as XML in a
> streaming fashion. As you can see, I'm trying to connect the source
> body to the `parseXML` function from `xml-conduit`, but I get the
> error message below. If anyone could point me in the right
> direction, I'd appreciate it.

Make sure to include the import statements and language extensions
too, so that anyone reading can just copy-paste your example and get
the same error message. (I like using
[reproducible Stack scripts](https://haskell-lang.org/tutorial/stack-script)
for this.) You may notice an overlap with _minimal repro_ from above:
that's intentional.

## Help other people

If I see people answering questions on a mailing list or Stack
Overflow, I'm appreciative. I consider them a comrade-in-arms. And I
consider them assets to the community. They've earned my respect, I'm
indebted to them, and I want to entice them to continue. Honestly: all
of you helpers are awesome in my book. If one of those people asks a
question, I want to help more.

In addition to all of the feelings I mentioned above, there's also a
more basic one: if _this_ person is having trouble, it's probably not
the most basic, boring question. In reality, the person may be barely
a beginner, and may be asking beginner questions. But I know
statistically that just having that helpful person's name associated
with the question increases the likelihood of it being interesting. In
other words: I'm nerd sniped.

This points out something which really applies to all of these
sections: people have memories. As soon as you start interacting with
the community, you're building a reputation. You can change that
reputation over time (for better or worse), but you have to
acknowledge that it's there.

## Don't be rude

Compare:

> Thank you for your great software, I really appreciate the time
> you've taken to make it. I'd appreciate your help with...

With:

> I've been pulling my hair out trying to parse your documentation for
> the past two days. You think you can help me make sense of it? I'm
> trying to...

And even further:

> Since I'm stuck using your piece of s*&# software with crappy
> documentation, the least you can do is help me overcome...

If your goal is to get someone to help you, I'd place a large wager
that the first approach will attract the most assistance. It doesn't
matter if the other two approaches really do capture your current
mental state. Are you trying to vent at someone, or get help?

I recently had someone argue a few times that the tone of a question
shouldn't matter. (In fact, those interactions were partially what
encouraged this blog post.) I argue that that's not only naive, but
dangerous:

* We're human beings, and we like being treated nicely. As I mentioned
  above, open source community members are giving up a portion of
  their lives to help strangers. You should make that sacrifice feel
  as rewarding as possible.
* Rude comments like this scare other people away. Encouraging people
  to continue with them by rewarding them with a helpful answer has
  the real possibility of scaring away more constructive community
  members.
* All of life is a series of choices between different things I can be
  doing. If you make it miserable enough to interact with you, I may
  very well choose "watch paint dry and see this jerk badmouth my
  project in public" over "give this guy any more of my time."
* Whether correct or not, being rude is a signal to me of other likely
  tendencies. I'm likely to guess that someone rude is also selfish,
  unwilling to minimize time wastage for others, and unlikely to
  contribute back by helping people. If I have to make a snap
  judgement on you based on a question and your tone of voice, a rude
  tone is not going to help you.

I honestly haven't found the best approach to this specific
problem. In some cases, a private message saying "your message would
be more well received if you modified it" can help. But if I'm honest,
by the time I think about writing such a message, I've basically
decided that this is a person not worth my time, and trying to
encourage him/her to behave better isn't worth it.

The situation is slightly different if someone has been in the
community for a while, and suddenly has an outburst of rudeness. I'm
not excusing it, but I am saying I'd be more willing to consider that
he/she is having a bad day, or that the problem is really bad, instead
of "this person is just a jerk." Also, the reverse is true: if you've
been rude to someone for the past 10 interactions, it may be very
difficult to convince them to help you on the 11th, even if the
rudeness disappears. (Overwhelming _niceness_, however, can help.)

Side note: I implied above that the project documentation sucks. That
may be the case. See "offer to help" for advice on pointing that out.

## Say thank you

I'll preface this one with a few caveats so I don't get a flurry of
guilt-ridden "thank you" notes. Most people don't say thank you in
open source. I rarely write a thank you note to a package author. I
don't expect it, I've never met anyone who expects it, it is not
necessary.

When someone has received assistance on a mailing list, I get
happy. When that person responds with a sincere thank you, I get
happier. When I'm the person who did the helping, I'm even happier
still. It's simple, it's trivial, but it's often missed. Most people
are only doing open source work to help others. Gratitude may be their
only reward.

Taking it a step farther: there have been a few times over the years
where, out of nowhere, I've received a very kind personal email
thanking me for work I've done. You can
[ask my wife](https://twitter.com/LambdaMom) and she'll confirm: it's
truly touching to receive such messages.

I know I've had views of open source maintainers as being far beyond
the lowly likes of me in the past. I don't think it's generally
true. Most people are, at the end of the day, just people. And they
respond like any other people to kind words.

Though I have a feeling that Linus Torvalds may be a bit confused if
you pop him an email saying "love Linux, thanks!"

## Admit if you're new

This one works one time per community. If you're new, you don't know
what you're doing, and are asking for help, say straight out that
you're new. It will get you some sympathy (unless you're lying, then
people will hate you). It will get a more softball answer, and likely
some guides to places explaining how to interact better. For example:
if you come to the Yesod issue tracker and say "I'm new, I'm not sure
if this is the best place to ask about installing GHC," you'll likely
get pointed to an install page and Stack Overflow for further "please
help me" questions.

## Offer to help

This may be the first surprising piece of advice. Let's say the docs
on my library suck. You _could_ come in and say "help me solve X
because your docs suck." And I might answer. Now consider this:

> I was having trouble doing X with your library (thank you for it by
> the way!). I'd be happy to prepare a documentation PR to help other
> people in my situation, if you'd be able to guide me towards an
> answer.

Whoa, what is this? Help someone and they'll take away the dreaded
documentation writing task from my plate? Awesome, I'll get right on
it!

In addition to docs, similar thoughts apply to:

* Offering to write test cases
* Offering to add some missing functionality
* Offering to answer people's questions on other issues/the mailing
  list/Stack Overflow

The point is: convince the maintainer (or whoever) that giving time to
you is an investment.

## Give money

This isn't at all a universal one. And to be clear: I'm not asking for
it, if I have an envelope with unmarked bills on my doorstep tomorrow,
I'll be weirded out.

Some people just need money. They like contributing to open source
work, but they have to pay the bills. If they've at all expressed a
willingness to accept money for their work (like setting up Flattr or
Patreon or whatever is popular these days), considering donating.

Consider how much of their time you're taking. Consider how much of
your time they would be saving you. Consider what a typical software
developer hourly rate is. And then realize that buying someone a beer,
or even a nice dinner, is probably a cheap price to pay for an answer
to a question.

And for those who aren't asking for any money, offering to buy the
beer/coffee/soda when you meet up at a conference is a nice way to
make this one a reality too.
