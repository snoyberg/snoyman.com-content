I find myself repeating a lot of the same comments in pull requests,
so I decided to put together a list of what I consider the most
important features of a good pull request. Other people will have
different feelings on some of these, but the points below are what
apply to my projects. If you have thoughts on things I've left out, or
reasons why you disagree with these points, please comment below.

Many of these points only make sense for source code, and even more
specifically for code written in Haskell. My content repos (like
[this site's content](https://github.com/snoyberg/snoyman.com-content))
and non-Haskell repos (do I have any of those???) would be slightly
different.

__NOTE__: I'm _not_ the maintainer of Stack, so the comments below do
not necessarily apply there. Stack has its own contribution rules, so
please don't take my personal opinions here as relevant to that
project.

* Every top-level identifier exported from a library needs to have a
  Haddock comment. It's irrelevant if the identifier name is
  completely self-commenting; a comment is still necessary. (These are
  the comments that look like `-- | Describe what the function does`.)
* My packages all follow PVP-style version numbers, and pull requests
  should include bumps to the version number. For those unfamiliar:
  PVP version numbers consistent of four components, A.B.C.D.

    * If your change fixes a bug without modifying the API at all,
      then bump D (1.2.3.4 becomes 1.2.3.5, or 1.2.3 becomes 1.2.3.1).
    * If your change adds something new to the API without changing
      something that already exists, bump C (1.2.3.4 becomes 1.2.4).
    * If you change the existing API (e.g., remove a function, change
      semantics, modify a data type), bump either A or B (1.2.3.4
      becomes either 1.3.0 or 2.0.0, depending on how big a change you
      think this is).

        * By the way, I'm unlikely to include a breaking change unless
          you have a really good reason. I consider backwards
          compatibility really important. Consider exporting a new
          function and (optionally) deprecating the old one instead.

* To elaborate on motivation for the previous point: I follow a policy
  of releasing new code fairly quickly in most cases, as opposed to
  batching up a number of changes. In that situation, it makes sense
  for a PR to include the new version number immediately. Many other
  projects work differently, and do _not_ encourage contributors to do
  version bumps.

    * Also, I sometimes forget to make a new release after merging a
      PR. If I do forget, don't be shy about pinging me to make a
      release.

*   Include a `@since` annotation in each new identifier, including
    the new version number you just bumped to. This is absolutely
    vital for users of a library to properly specify lower bounds in
    their dependencies easily.

    * Don't include `@since` for un-exported identifiers.
    * If you are exporting a pre-existing identifier that was previously not exported, include the `@since`.
    * In other words: `@since` indicates when it was added to the
      public API, _not_ when it came into existence.

    Example:

    ```haskell
    -- | Download a JPEG image.
    --
    -- @since 1.5.2
    downloadJPEG :: MonadIO m
                 => Request -- ^ URL to download from
                 -> m JPEG
    ```

*   Include a `ChangeLog.md` entry. If the project doesn't have a
    `ChangeLog.md`, create one. Odds are the content you write in the
    changelog can be identical to the pull request description. This
    is a huge end-user convenience. Example:

    ```markdown
    ## 1.5.2

    * Added the `downloadJPEG` function
    ```

* Slight exception to the above: if you're making a doc-only PR, don't bother
  with a version bump, but instead add a ChangeLog entry with `## Unreleased`
  and a description of the change.

* Do not include unrelated changes in your PR, it makes it difficult
  to review, more likely to get delayed, and more likely to conflict
  with other changes. Include separate changes in separate PRs.
* Keep coding style consistent with the rest of the code. I'm not a
  big stickler for coding style guidelines in general, but I _do_
  consider it very jarring to have conflicting styles in the same
  file, or even worse the same function.
* Similar to the previous point: think hard before sending a pull
  request to modify the style of code. Again, I'm not a big stickler
  on coding style, and I consider style in general a pretty arbitrary
  and unimportant aspect of the code. But that makes me even less
  likely to want to spend my time reviewing and discussing changes
  like whether records should be aligned. (For the record, I _don't_
  think they should be aligned, as it makes diffs noisier than
  necessary.)
* If you have a PR for addressing a typo, making a trivial fix, or
  adding a straightforward feature: just send a PR without any prior
  discussion. However, if you want to make a major overhaul, change
  behavior, or break API compatibility: it's worth opening an issue
  first to discuss. I don't like rejecting PRs or causing people to
  waste their time, so I'd rather hash out details before you spend a
  lot of time on something.

*   Don't use partial functions. I've received lots of PRs which, for
    example, use `fromJust` because "by analyzing the rest of the
    code, you can tell it will always be a `Just` value and not
    `Nothing`." That doesn't cut it for me:

    * I'm lazy, and I don't want to reason about the code. I want the
      compiler to do it for me.
    * I'm stupid, and I don't trust my own reasoning.
    * Such logic does not withstand future refactorings of the code,
      making it fragile.

    There are definitely some exceptions to this rule, but odds are
    pretty good your case won't be one of them :).

* Added test cases are always a good thing. They also make it easier
  for me to understand _why_ you're writing this PR, and ensure I
  don't break your work in the future (remember from the previous
  point: I'm stupid).

* When submitting PRs to libraries (as opposed to an application like
  haskellers.com):

    * Keep compatibility with older versions of dependencies whenever
      possible. I try to keep as broad a range of potential package
      versions as I can to help avoid "dependency hell." (For the
      record: dependency hell is not related to tooling in any way,
      it's an intrinsic aspect of having dependencies.)
    * In some cases, if you drop compatibility with a major version of
      a dependency (e.g., change from `transformers >= 0.3` to
      `transformers >= 0.4`), I may consider it a breaking change in
      the library worthy of a major version bump.
    * Avoid adding dependencies. I personally am not of the opinion
      that reducing the dependency footprint is _that_ important, and
      strongly believe that such behavior in general leads to Not
      Invented Here (NIH) syndrome. However, since enough users of my
      libraries _do_ feel this way, it's easier on me if you don't
      incur unnecessary dependencies.
