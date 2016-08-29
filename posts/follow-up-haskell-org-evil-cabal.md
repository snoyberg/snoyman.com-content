Yesterday I
[put out a blog post](http://www.snoyman.com/blog/2016/08/haskell-org-evil-cabal)
describing a very problematic situation with the haskell.org
committee. As often happens with this kind of thing, a
[very lively discussion occurred on Reddit](https://www.reddit.com/r/haskell/comments/4zzmoa/haskellorg_and_the_evil_cabal/). There
are many repeating themes over there, so instead of trying to address
the points in that discussion, I'm going to give some responses in
this post.

*   Firstly: thank you to those of you who subscribed to the
    haskell-community list and made your voices heard. That was the
    best response to the blog post I could have hoped for, and it
    happened. At this point, the
    [Twitter poll](https://twitter.com/snoyberg/status/769732460879962112)
    and mailing list discussion both point to a desire to have Stack
    as the primary option on the downloads page (the latter is a tied
    vote of 6 to 6, indicating the change proposed should not
    happen). As far as I'm concerned, the committee has two options:

    * Listen to the voices of the community and make Stack the primary
      option on haskell.org.

    * Ignore the community voices and put the Haskell Platform at the
      top of the page, thus confirming my claims of an oligarchy.

*   Clarification: I do not believe anyone involved in this is an evil
    person. I thought my wording was unambiguous, but apparently
    not. The collusion among the projects is what gets the term "Evil
    Cabal." That said, I _do_ believe that there were bad actions
    taken by individuals involved, and I've called some of those
    out. There's a much longer backstory here of the nepotism I refer
    to, starting at least at ICFP 2014 and GPS Haskell, but that's a
    story I'm not getting into right now.

*   A few people who
    [_should_ know better](https://twitter.com/snoyberg/status/770085132493545476)
    claimed that there's no reason for my complaint given that the
    Haskell Platform now ships with Stack. This is incorrect for
    multiple reasons. Firstly, one of my complaints in the blog post
    is that _we've never discussed technical merits_, so such a claim
    should be seen as absurd immediately. There's a
    [great Reddit comment](https://www.reddit.com/r/haskell/comments/4zzmoa/haskellorg_and_the_evil_cabal/d70pffl)
    explaining that this inclusion is just misdirection. In any event,
    here are just
    [140 characters worth of reasons the Haskell Platform is inferior to Stack for a new user](https://twitter.com/snoyberg/status/770092223748837378)

    * There is no clear "getting started" guide for new users. Giving
      someone a download is only half the battle. If they don't know
      where to go next, the download it useless. (Compare with
      [haskell-lang's getting started](https://haskell-lang.org/get-started).)

    * Choice confusion: saying "HP vs Stack" is actually
      misleading. The real question is "HP+cabal-install vs HP+Stack
      vs Stack". A new user is not in a strong enough position to make
      this decision.

    * Stack will select the appropriate version of GHC to be used
      based on the project the user is working on. Bundling GHC with
      Stack insists on a specific GHC version. (I'm not arguing that
      there's no benefit to including GHC in the installer, but there
      are definitely downsides too.)

    * The HP release process has historically been very slow, whereas
      the Stack release process is a well oiled machine. I have major
      concerns about users being stuck with out-of-date Stack
      executables by using the HP and running into already fixed
      bugs. This isn't hypothetical: GHC for Mac OS X shipped an old
      Stack version for a while resulting in many bug reports. (This
      is an example of haskell.org download page decisions causing
      extra work for the Stack team.)

    * Bonus point (not on Twitter): Stack on its own is very well
      tested. We have little experience in the wild of HP+Stack. Just
      assuming it will work is scary, and goes against the history of
      buggy Haskell Platform releases.

*   A lot of the discussion seemed to assume I was saying to get rid
    of cabal-install entirely. In fact, my blog post said the exact
    opposite: let it continue if people want to work on it. I'm
    talking exclusively about the story we tell to new users. Again,
    technical discussions should have occurred long ago about what's
    the best course of action. I'm claiming that Stack is by far the
    best option for the vast majority of new users. The committee has
    never to my knowledge argued publicly against that.

*   There was a lot of "tone policing," saying things like I need to
    have more patience, work with not against the committee, follow
    the principle of charity, etc. If this is the first time I raised
    these issues, you'd be right. Unfortunately, there is a long
    history here of many years of wasted time and effort. The reason I
    always link back to
    [pull request #130](https://github.com/haskell-infra/hl/pull/130)
    is because it represents the tipping point from "work with the
    committee without making a fuss" to "I need to make all of these
    decisions as public as possible so bad decisions don't slip in."

    Let me ask you all: if I had just responded to the mailing list
    thread asking for a different course of action to be taken, would
    most of you know that this drama was happening? This needed to be
    public, so that no more massive changes could
    [slip under everyone's radar](https://github.com/haskell-infra/hl/pull/122).

    Also: it's ironic to see people accusing me of violating the
    principle of charity by reading my words in the most negative way
    they possibly can. That's true irony,
    [not just misrepresenting someone's position](https://www.reddit.com/r/haskell/comments/4zzmoa/haskellorg_and_the_evil_cabal/d70bysm).

*   For a long time, people have attacked FP Complete every chance
    they could, presumably because attacking a company is easier than
    attacking an individual. There is no "FP Complete" conspiracy
    going on here. I decided to write this blog post on my own, not
    part of any FP Complete strategy. I discussed it with others, most
    of whom do _not_ work for FP Complete. In fact, most of the
    discussion happened
    [publicly, on Twitter](https://twitter.com/snoyberg/status/769732460879962112),
    for you all to see.

    If you want to attack someone, attack me. Be intellectually
    honest. And while you're at it: try to actually attack the
    arguments made instead of resorting to silly ad hominems about
    power grabs. Such tin-foil hattery is unbecoming.

*   There's a legitimate discussion about how we get feedback from
    multiple forms of communication (mailing lists, Twitter,
    Reddit). While that's a great question to ask and a conversation
    to have, it really misses the point here completely: we're looking
    for a very simple vote on three options. We can trivially put up a
    Google Form or similar and link to it from all media. We did this
    just fine with the FTP debate. It feels _almost_ disingenuous to
    claim that we don't know how to deal with this problem when we've
    already dealt with it in the past.
