NOTE: This guide is essentially me taking notes while I learn about
the matrix.org/riot.im projects. Some of it is almost certainly
wrong. And I'm trying to take notes on how an end user uninterested in
the technical underpinnings work. There is clearly _much more_ to
these projects than I imply here. If you have suggestions for
improvements, please
[send a PR](https://github.com/snoyberg/snoyman.com-content/edit/master/posts/guide-to-matrix-riot.md).

As anyone on the internet or a cell phone knows, there are very, very
many different and competing messenger applications out there. I
recently started using Matrix and Riot. I didn't find a good "getting
started" guide to these systems, and so decided to write down my
findings here in such a setup.

I'm not going to belabor this point too much, but I want to at least
acknowledge the question: why should I start using a new system when
so many other, more popular options exist? And especially one that
apparently requires a guide to use correctly! Here is my quick list of
reasons why I started using these systems:

* Fully open source tooling and open protocols: no lock in at all
* Potential to use alternative, lighter-weight clients if desired
* Encryption
* Full decentralization

If these reasons don't seem important to you, you may want to stick
with more mainstream tooling. I'm particularly interested in these
options for open source project communication, which is also why I'm
writing this blog post (to help others onboard more easily). More on
that topic in a separate blog post.

## Matrix.org vs Riot.im

Matrix, hosted at [matrix.org](https://www.matrix.org), is "An open
network for secure, decentralized communication." It is a protocol
people use to communicate with each other. There is a concrete server at
matrix.org that speaks this protocol, but you're free to run your own
server on your own machine. That's the "decentralized" aspect of
things.

[Riot](https://riot.im) is the "default" application for talking with
Matrix. It's a web app, a Desktop app (via Electron), and a mobile
app. Just like the server, the client has
[alternatives](https://matrix.org/docs/projects/try-matrix-now.html)
as well, including text-based interfaces.

Since I'm going for simple overview of the common use case here, I'm
going to assume you're using the matrix.org server and the Riot.im
client.

## Identifiers/entities

As you'd expect, there are users on Matrix. You can create an account
with Riot and get a username. I did this myself, and created my
standard username: `snoyberg`. If Matrix was a more standard system,
you may think you could find me as `@snoyberg`. However, that's not
the case. Remember, there are different servers in the network, and
`matrix.org` is just one of them. Therefore, we need to state which
server I'm a part of. Therefore, my identifier is
`@snoyberg:matrix.org`.

There's a convenient site, [matrix.to](https://matrix.to), which will
allow you to create URLs linking to specific entities. When you visit
these pages, you'll see links to communicate with those entities. If
you enter `@snoyberg:matrix.org` on `matrix.to`, you'll get the URL
[https://matrix.to/#/@snoyberg:matrix.org](https://matrix.to/#/@snoyberg:matrix.org).

Besides users, there are two other entities you can create and
interact with:

* Chat rooms, which begin with `#`
* Communities (aka teams and groups), which begin with `+`

As two concrete examples:

* [https://matrix.to/#/@commercialhaskell:matrix.org](https://matrix.to/#/@commercialhaskell:matrix.org)
* [https://matrix.to/#/+commercialhaskell:matrix.org](https://matrix.to/#/+commercialhaskell:matrix.org)

__Summary__ You can use https://matrix.to to create links to
entities. Entities must include the server they are a part of, which
will usually be `:matrix.org` at the end. The `@` prefix is for users,
`#` is for channels, and `+` is for communitites.

## Rooms

Rooms are places where multiple people can have a conversation. You can create a new room with the icon with a `+` sign in the bottom-left corner of Riot. You can give each room a name when creating it. Once you're in the room, you can invite people via the "Invite" icon in the bottom right.

You can invite via email address. If they aren't already on Matrix,
they'll get an invitation email to sign up. You can also invite via
ID. Remember, if you wanted to invite me, don't just use `snoyberg`;
you need to use `@snoyberg:matrix.org`.

You can click on the gear icon in the top-middle of the screen to open
a bunch of settings for a room. Changing the name and topic of the
room are fairly self-explanatory. To me, the most interesting settings
are around privacy.

You can change who has access to a room. By default, each person must
be invited to the room. You can also allow people to join via a
link. In order to do this, you need to create an address for a
room. This would be something like `#myroomid:matrix.org`. For
example, I created a room
[#commercialhaskell:matrix.org](https://matrix.to/#/#commercialhaskell:matrix.org).

You can also choose whether a room will be listed in the Matrix
directory.

## Communities

Communities seem to be a relatively new feature, and not all clients support them yet. It seems to me that they are adding two nice things:

* Create a description for a community
* Add a bunch of related rooms

I'm still exploring this feature, but I think it will fill the exact
niche I need for my open source project maintenance. If you're
interested, you can check out the
[Commercial Haskell community](https://matrix.to/#/+commercialhaskell:matrix.org)
I created.

## Bridging

This seems to be a "killer feature" for Matrix. There are _lots_ of
apps available for it to bridge with other systems, including IRC and
Slack. For example,
[#haskell:matrix.org](https://matrix.to/#/#haskell:matrix.org) is a
bridge to the #haskell channel on Freenode (IRC).

For open source work, I'm hoping that this kind of bridging will allow
people who prefer IRC to interact with those looking for clients with
a different interface.

__UPDATE May 15, 2018__ I found a great guide that demonstrates how to
get tighter integration with IRC:
<https://opensource.com/article/17/5/introducing-riot-IRC>.
