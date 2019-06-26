I didn't mention it on this blog, and I think not even on Twitter. But
back in October of 2018, we started a major renovation project on our
house. The project ended up taking seven months, and we just moved back in two weeks ago. Two special thanks are in order:

* A big thank you to Miriam, who moved us back home while I was off at
  LambdaConf for a week
* A big thank you to my parents for letting Miriam and me, plus four
  children, invade their house for seven months straight

Anyway, most of the changes we were attempting are fairly
straightforward, non-technical, and (besides to those of us living in
the house) uninteresting. Things like replacing leaking pipes, adding
insulation to the walls so we don't freeze in the winter, and so on.

But this work also include a major change to how our home network
works. I'm far from an expert on anything hardware related, including
wired networking and wireless devices. This process involved a lot of
investigation on my part, and trying to plan out choices many months
in advance. I consulted with friends, especially coworkers at FP Complete who are more knowledgeable about this stuff than me. So far, I'm very happy with the end result.

I'm writing up this blog post for people who are curious about how
this setup ended up looking, and in case it may help others make some
decisions.

## Requirements

Something about my house: it's tall. We live on the side of a
mountain, and so we have 4 split levels. This makes it difficult to
get a good WiFi signal across the whole house. Originally, before the
renovation, I bought a D-Link COVR 1203 set of devices (1 router, 2
access points). These devices work with a wireless backhaul, meaning
they communicate with each other over a wireless as opposed to a wired
connection. This worked, but we always had deadspots in the top level,
and much degraded speed. So we wanted something better. (Those devices
are now living happily at my parents' house, and were a lifesaver for
surviving there for the past seven months.)

Anyway, here's a breakdown of what we need:

* First and foremost, the majority of my work revolves around voice
  and video calls. I need a 100% reliable connection for that,
  otherwise I have major disruptions in my ability to do my job. I
  have an office on the second floor, but also sometimes need to work
  from our bedroom on the fourth floor when we have guests staying
  with us. For me, both wired and wireless connections are an option.
* We have a TV in the living room on the first floor, previously
  connected to a Chromecast, now connected to a Roku. (I strongly
  recommend Roku devices, we've been really happy with this one.) We
  stream on this Roku, and it needs a solid signal. We had a lot of
  frustration with flaky wireless to the Chromecast before.
* Beyond those key points, we overall want solid wireless throughout
  the house, which probably isn't too much of a surprise. It's not a
  hard requirement to be able to use WiFi everywhere in the house, but
  it's nice to have.
* Finally, as a nice to have, we'd like to have the option in the
  future of getting our kids desktops in their bedrooms and setting up
  wired connections.

That all sounds pretty standard I'm guessing. Some things that aren't
required:

* We don't have any special router requirements. All devices are OK
  with a standard IPv4 NAT setup. No special firewall rules are
  necessary.

OK, let's talk about what we did.

## Modem and router

We set up both phone line and cable line jacks in the living room
right by the TV. This area is our entertainment center, with the TV,
Roku, and Nintendo Switch. So adding in a modem was natural. We went
with the cable company HOT. I really dislike the company... but what
can you do, every company in this space seems to suck.

<img alt="Living room entertainment center" src="/static/images/network/entertainment-center.jpg" style="max-width:100%">

They gave us what they call a Hot Box (heh), which is a pretty
standard modem + router + WiFi access point combo. For those not
familiar with the distinction between these:

* A modem converts an analog signal (like a television cable line) to
  a network signal. It allows a single device on each end of the line
  to talk to each other.
* A router allows multiple devices on a local area network to connect
  to a wide area network (usually the internet). It performs tasks
  like a DHCP server, network address translation (NAT), and
  firewalling.
* A wireless access point allows devices to connect to a network over
  WiFi instead of over a wired connection.

The wireless access point in the Hot Box was terrible: it was slow to
connect and used an old wireless standard (more on that later). I
disabled that feature. However, since I didn't have advanced rotuer
needs, we're using the router capabilities of the Hot Box.

## Cables in the walls

The magic of all of this is, now that we were doing construction, we
had a chance to run network cables through all of the walls of the
house. Our electrician ran three CAT6 cables through the house: one to
my office on the second floor, one to one of the kids' bedrooms on the
third floor, and one to our bedroom on the fourth floor. All of the
cables originate in the same cabinet holding the Hot Box.

The Hot Box provides four network jacks. As you'll see, we have more
than four devices in the house that want a wired connection. So the
next addition to this show is...

## Network switches

A network switch is a device which allows multiple devices to connect,
and will route packets between them intelligently. For our purposes, a
switch allows us to make one connection to the Hot Box, and then plug
in multiple devices to its other ports, resulting all connected
devices having an internet connection.

We purchased TP-Link 5-port switches. The reason for this choice was a combination of reading reviews, price, and availability of the product in Israel. I'm happy with them, and have no complaints at all, but have no reason to believe them to be qualitatively better than other devices on the market.

<img alt="TP-Link Switches" src="/static/images/network/network-switch.jpg" style="max-width:100%">

By adding a switch next to the Hot Box itself, we ended up with 4 + 5
= 9 ports. However, connecting the two devices uses a port on each
device, so we had seven usable ports. Each of the CAT6 cables to the
other three floors got their own port. The Roku gets a port. The
Nintendo Switch (naming here is really confusing) will ultimately get
a port, once we buy an ethernet adapter for it. And as we'll see in
the next section, the wireless access point gets one too.

In my office, I put in a network switch as well, since I'll be
connecting more than one device on a wired connection in
here. Similarly, in the kids room, I put in another network
switch. From the one bedroom, we have a network cable running into a second
bedroom, so that both rooms end up with a wired connection without
having to run two cables through the walls of the house.

I'm honestly not convinced I made a great decision on this, and
perhaps running extra cables and buying a single, larger switch for
the living room would have been smarter. But this definitely works.

With this much of the setup, we now have the ability to have a wired,
stable, fast connection for my laptop in our bedroom, for a proper
workstation in my office, for the kids' computers in the future, for
streaming videos on our Roku, and for playing Nintendo Switch games
online. (I still have to have my brother over to test out Fortnite at
some point.) But there's no wireless connection yet, so...

## Wireless access points

This is the area I least understood before starting this project. I
still won't claim to be an expert, and I'll be happy to modify this
section if people have corrections (or just send a PR using the link
above). But here's what I've learned.

The basics of a wireless access point (WAP) are simple. They provide
wireless devices with something to talk to wirelessly. They then route
packets from those devices to others. Arguably the simplest kind of
WAP will have a wired connection to a router or switch, and provides a
bridge between the wired and wireless connections. Other kinds of
access points work differently, such as by connecting to some other
WAP for the internet signal. (This is what my COVR devices with
wireless backhaul did, for instance.)

I did not want to deal with a wireless backhaul, since it has multiple
limitations. Firstly, the effective speed is cut in half, since half
the wireless bandwidth needs to be used to talk to the source
signal. Also, The devices need to be close enough together to talk,
which is a problem with our house. And as you'll see shortly, with the
distances in my house, this would drastically degrade the speed.

So instead, the goal with this renovation was to have a WAP with a
wired connection on each floor, which would provide complete house
coverage and no issues with degraded signal due to distance between
the devices. Having determined the kind of device to look for, I
started researching the topic.

The next thing I learned&mdash;which I'm sure many readers already
knew&mdash;was about the difference between the 2.4Ghz and 5Ghz
bands. Successive wireless standards, like 802.11n vs 802.11ac, have
added the ability to communicate over 5Ghz instead of 2.4Ghz. 2.4Ghz
is advantageous in that it can work over longer distances and more
easily penetrate obstacles like walls. By contrast, 5Ghz offers faster
speeds. Given that distance was hopefully no longer going to be an
issue, getting devices with support for 802.11ac was advantageous.

With the same review process and criterion as I used for the network
switches, I ended up choosing the TP-Link EAP245. This is a business
class solution, and likely overpowered for our needs, but I'd rather
get this set up once and not have to think about it again for
years. This requires more tweaking than the network switches, but the
interface was pleasant to use and the mobile application intuitive.

<img alt="TP-Link EAP245" src="/static/images/network/wireless-access-point.jpg" style="max-width:100%">

So far, everything's been great with these devices. I've done signal
checks throughout the house, and everywhere I've checked we have well
above 80% signal (usually closer to 95%) on the 5Ghz band. A speed
test in my bedroom topped out at 195Mbps, over a cable line providing
a maximum of 200Mbps. So I think it's fair to say that the wireless
signal is no longer the bottleneck in the house. In fact, I also
tested the network speed using a cheap WiFi dongle I bought years
back, and it was significantly slower than the wireless connection.

These devices include some features that are either really great or
awesome snake oil. "Fast roaming" means that, for client devices that
support it, there will be a seamless transition between two of the
access points when moving through the house. This can be great in
theory, but I haven't tested it in practice yet.

I set up the devices to use the same SSID for both the 2.4Ghz and 5Ghz
bands. I read conflicting information on whether this was a good idea
or not. The advantage for me is simpler configuration of devices (no
need to tell guests which network to use), and in theory devices will
automatically switch between 2.4Ghz and 5Ghz depending on how far they
are from the respective signals.

It's been a complete pleasure having reliable WiFi on all our devices
throughout the house.

### Power over Ethernet

As a side note, the WAPs do not have a power port. They have two
network ports only: an incoming port for the internet signal and
power, and an outgoing port for sharing a signal with another device
if desired. I was unfamiliar with Power over Ethernet (PoE) before
this project, but it's pretty nifty. In our bedroom, it means the WAP
has just a single cable going into it from the wall, with the PoE
injector living downstairs next to the Hot Box itself.

## Network attached storage

This was a nice little perk I hadn't expected. We have a portable USB
hard drive with movies and music, which we connect to our Roku. Any
time we want to transfer files, we unplug it, bring it to one of our
computers, plug it in, do the transfers, and then reconnect it to the
Roku. I feel like an [absolute
caveman](https://en.wikipedia.org/wiki/First_World_problem) doing
this!

Now that our in-house bandwidth is so high, we started playing with
the idea of buying a Network Attached Storage (NAS) device so that the
Roku could stream off of it over the network, and we could transfer
files to it from our computers. However, to my surprise, the Hot Box
itself has such capabilities: I plugged the USB hard drive into the
Hot Box's USB port, and was immediately able to access the drive from
both the Roku and our laptops. This was a great little perk, and being
able to watch movies or listen to music from multiple devices at once
is awesome.

## Cost

I don't have all the figures at my disposal, but overall, the costs
involved in this setup were:

* Network switches and WAPs: about $612. I ended up buying extra
  devices due to a sale at the company I purchased from, and ended up
  with 5 of each, even though I only needed 3 switches and 4
  WAPs. Happy to have the backups.
* Since we were already doing renovations and fixing lots of
  electrical work in the house, running the three CAT6 cables wasn't a
  major expense. I think it was order-of-magnitude less than $200
  total.
* Additional network cables for connecting the switches to each other,
  the PoE injectors to the switches, and the WAPs to the PoE
  injectors, cost me around $60. This turned out to be many more
  cables than I was expecting, and I cleaned out the local hardware
  store of their 1 meter cables. Twice.

Considering how vital all of this is to my job, that's an easily
absorbable cost. And the joy of never fighting with a flaky Chromecast
again are well worth it!

## Problems

Most everything went off without a hitch. Ultimately, I think I asked
the electrician to put the network jack in our bedroom in the wrong
location. Now that we're using it just for the WAP, having it higher
on the wall would have been less intrusive. I'm also pretty terrible
at making jumbles of cables look nice, so there's still some cleanup
in my office and the kids' rooms to make it presentable.

## Conclusion

I planned out all of this towards the beginning of our construction. I
then got to wait around for seven months to see if the plans were
going to work out in practice correctly. It's a huge relief to find
out that it did.

I would strongly recommend that anyone doing serious renovations
consider running network cables through their walls at the same
time. And certainly, for any new construction, make sure you include
network cables along with power lines.

If I discover any problems or improvements, I'll try to blog about
them too.

I hope people found this useful, or at least interesting.
