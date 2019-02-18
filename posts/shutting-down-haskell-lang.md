Early this week, I merged a commit which essentially shuts down the
haskell-lang.org website. Besides a few rarely viewed pages without
obvious replacements, visiting pages on https://haskell-lang.org will
automatically redirect you to an appropriate page on
https://haskell.fpcomplete.com. Also, consider this an announcement
that there's a new site around, https://haskell.fpcomplete.com!

The site is still being refined. However, to avoid confusion, someone
requested that I make an announcement now. That request makes
sense. In this post, I want to explain:

* Why I shut down haskell-lang.org
* What the goal of haskell.fpcomplete.com is
* What makes this new site different from haskell-lang.org
* Plans for the future

Onward!

## Shutting down haskell-lang.org

I've been playing with the idea of shutting down haskell-lang.org for
a while now. I didn't want to simply turn it off, since there's a lot
of useful content there that I regularly use myself and point others
to for training (we'll get to that in a bit). It wasn't a huge amount
of work to put together the replacement site, but it did take some
dedicated writing time. I got that time last week and did most of the
work then.

I first publicly proposed shutting down the site [in December of last
year](https://www.snoyman.com/blog/2018/12/improving-commercial-haskell). I
put together a proposal to get a bunch of people together to work on a
new Commercial Haskell site. There wasn't much interest in such
collaboration, so I went with option B, which I'll explain in the next
section.

As for why haskell-lang.org should be shut down, I'll quote [the
history section of the new site's
README](https://github.com/fpco/haskell.fpcomplete.com/#history):

> This website replaces some previous false starts (and mistakes) at
> trying to create an opinionated hub of Haskell
> information. Previously, some FP Complete members participated in
> haskell-lang.org as another vision of an opinionated Haskell
> site. Later, we sought out collaborators for creating a more
> complete Commercial Haskell website.
>
> haskell-lang.org was a mistake, and there was insufficient interest in
> commercialhaskell.com. Given that the branding for haskell-lang.org was
> incorrect, there was little interest from others in the general concept
> of creating an opinionated site, and we at FP Complete were still passionate
> about this topic, we decided to focus efforts on a site
> under our own branding and control.
>
> This site is unapologetically opinionated, and follows what we have
> found to be the best route towards getting productive with Haskell
> quickly.

## The need for a site

haskell-lang.org has been collecting solid tutorials on general
Haskell techniques and specific libraries. The collection is
opinionated, so that I can use it for training courses I give, and
point new users to it without needing to give any caveats about which
approach to follow. And the site is backed by a Git repository using
Markdown files, which [I'm a huge fan
of](https://www.yesodweb.com/blog/2015/08/thoughts-on-documentation).

So first goal of this site would be: retain the technical content and
educational approach provided by haskell-lang.org, without the bad
history that goes along with that name.

## Other goals

At FP Complete, we've done a few surveys of the Haskell community to
get an idea of what we can do to most help more companies adopt
Haskell. From our last survey, it seems that more educational content
is top of the list, followed by helping companies generally feel
comfortable adopting Haskell. I covered the education aspect above,
and we'll continue to put efforts into improving that
situation.

On the more nebulous adoption point, we're adding two more goals to
the new site:

* Provide [promotion](https://haskell.fpcomplete.com/promote)
  material: content that explains what makes Haskell a good choice for
  businesses, together with some potential drawbacks people should be
  aware of.
* Introduce a [Success
  Program](https://haskell.fpcomplete.com/success) at FP Complete,
  providing affordable commercial Haskell support including training
  and mentoring. We believe this may help more companies adopt
  Haskell.

Enrolling in the Success Program is a paid service at FP Complete
(though we are pricing it as low as we can, to maximize adoption
without actually losing money). We're hoping that the
presence of a clearly defined and publicly priced commercial support
package will help reduce perceived risk with Haskell further and allow
more adoption.

## The future

The new site is still a work in progress. Overall, the styling still
needs a bit of work, I want to refine the language some (and likely
scale back the prose). I also want to refresh a bunch of the technical
content to be in line with our current recommendations. This will also
affect the [Applied
Haskell](https://www.eventbrite.com/e/applied-haskell-by-michael-snoyman-lambdaconf-edition-tickets-53187843271)
training I'll be giving later this year. (Feel free to check out [the
current version of the course
online](https://github.com/fpco/applied-haskell#readme).)

I still have some questions up in the air, like whether we'll host a
blog on this site or simply continue all FP Complete blogging
activitiy on [our corporate
site](https://www.fpcomplete.com/blog). I've started putting together
a bit of a [philosophy
page](https://haskell.fpcomplete.com/philosophy) explaining the FP
Complete approach to commercial Haskell development, and that needs
expanding. And I'd like to get a more content on the [contribute
page](https://haskell.fpcomplete.com/contribute) to allow people to
find a project they can cut their teeth on.

I hope that this new site not only allows for creation of and access
to great Haskell content. I also hope that this is taken as a positive
message from the rest of the community and an indication of how we at
FP Complete, and I personally, intend to interact with the broader
Haskell community going forward. We'll remain opinionated on our
technical recommendations, as I believe we should be. But hopefully
this change in naming and purpose of the site remove any adversarial
nature to sharing these opinions.
