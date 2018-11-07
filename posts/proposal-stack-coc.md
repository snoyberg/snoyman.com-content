Technical disagreement is not only an inevitable part of software development and community building. It is a necessary and healthy component thereof. Respectful conversations around technical trade-offs are vital to improving software. And these conversations need to be handled with proper care.

I have failed at this in the past, and I've apologized for having done so. I've strived to do better since. I have encouraged&mdash;and continue to encourage&mdash;my friends, colleagues, and fellow developers to do the same, and I believe it has been successful.

I think the other extreme&mdash;avoiding all discussions of disagreements&mdash;is actively harmful. And unfortunately, I see significant confusion in the general Haskell community around what is and is not an acceptable form of technical disagreement.

Outside of my position on the Core Libraries Committee, I do not hold any official position in the leadership of Haskell. I've written open source libraries and educational material. I've worked on and maintained build tooling. I run a team of Haskell engineers at FP Complete. None of those positions grant me any authority to demand compliance from individuals with any code of conduct.

That said, at many points in the past, I _have_ reached out to individuals whom I've been told (or saw myself) were behaving in a non-ideal way. I've attempted to remedy those situations privately wherever possible, and have on rare occasions asked people to discontinue such behavior publicly.

I've tried to keep this process informal because, as mentioned, I'm _not_ any official authority. The exception to this would be my leadership at FP Complete, but I have a strong policy of avoiding the usage of my position to force those reporting to me to behave a certain way. I do not believe it is appropriate for me to leverage what essentially comes to someone's livelihood to demand their compliance with my wishes.

There's been a slow burn of public discussion over the years that has made me consider making a more formal process. But recently, I had a "straw that broke the camel's back" moment. Someone I like and respect expressed opinions that I think are antithetical to healthy community growth. I won't call anyone out or refer to those discussions, I'm just explaining why I'm doing this now.

## My proposal

I will not in any way claim to have an authority position for the Haskell community. As a result, I'm not going to make any statement for the Haskell community. As the founder of the Stack project, I think I have more of a right (and responsibility) to speak for that project. So I'm going to refer here to the Haskell Stack community. I'm hesitant to do so given that it may be seen as divisive, but I believe the alternative&mdash;trying to speaking for the entire Haskell community&mdash;is inappropriate. My intent is _not_ to be divisive.

I also have no ability to enforce any kind of behavior on Stack users _or_ contributors. Stack is open source: no matter what statements I make, anyone can use it. I could ask the rest of the maintainer team to block pull requests from specific people, but I believe that's also overstepping my authority, and so I won't be doing it.

Instead, I intend to publish two documents, following interaction with people interested in engaging with me on this process. Those two documents will be:

* A code of conduct. I do not intend to write one myself, but instead use a standard, "off the shelf" CoC. I did this about a year ago [for Yesod](https://github.com/yesodweb/yesod/blob/master/CODE_OF_CONDUCT.md), and it worked out well.
* A recommended communication guide. This is something I _do_ intend to write myself, with feedback from others. I intend to write this as an open source project, on a Github repo, accepting issues and pull requests. I intend this to address specific concerns of how the communication I've been involved with has broken down.

I am not demanding anything of anyone here. I'm not expecting anyone in other communities (whether within Haskell or outside of it) to participate or change their behavior. And as stated, I'm not willing or able to demand changes from within the Stack community.

What I _am_ hoping is that by clarifying these points:

* People unsure of how to communicate in certain situations have some clarity
* We have some better ideas of how to communicate on sensitive technical topics
* If someone within the Stack community is seen as misbehaving, there's a mechanism to politely and confidentially raise the issue, and hopefully have the situation improved

## Next steps

I've [created a Github repo](https://github.com/snoyberg/stack-coc). This repo is on my personal Github account, not `commercialhaskell`, `fpco`, or `haskell`. Again, this is my own personal set of content. I encourage others to participate if they feel invested in the topic. I'm considering having a sign-up sheet for the repo after we have a first version, so that people can state that:

* They've reviewed a certain version of the repo
* They agree with what it says
* They will strive to follow its recommendations

## What to avoid

We'll get into far more details in that repo itself, but since I
anticipate this blog post itself kicking off some discussion, I'm
going to make some requests right now:

* Avoid ad hominems. (Yes, I've made mistakes here in the past.) This applies both to someone's personal history, and to any organizations they are members of, or who they are friendly with.
* Avoid offensive language. This is actually more complicated than it seems; one of the ways I've upset people is my highly sarcastic-by-nature communication style. I've worked hard on tamping that down. I point this out because what's offensive to one person may be normal to others. This is _especially_ true in a global community.
* We will inevitably have to address some old behavior to know what to include in our recommendations. This is _not_ an invitation to air grievances, as tempting as that may be for some people. Keep the comments non-personal, speak in general terms, and try to avoid any grandstanding. This will be a tough line to walk. But a simple example:
    * Good: "I've seen people discredit some people's opinion because of who their friends are, I believe we should address that in the guidelines."
    * Bad: "Two months ago, in _this Reddit thread_, person X said Y about Z. This is horrible, and X needs to either apologize or be removed from the community. They need to be mentioned by name in the guidelines as an offender."

This is my first time trying to have a discussion quite like this, and I'm guessing the first time many people involved will be participating in one. As one final note, I'd like to request people make an assumption of good will. Mistakes will be made, but we can try to minimize those mistakes, and move on from them when they occur.
