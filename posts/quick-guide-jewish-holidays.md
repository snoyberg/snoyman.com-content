At various points in the year, I end up sending out emails to
colleagues about some unavailability in my schedule due to the Jewish
holidays. These emails are always fairly brief, and don't give all of
the explanation about:

* Why I'm available on some holidays, but unavailable on others
* Why the holidays seem to move throughout the year
* Why I'm always unavailable starting the day before the holiday

I'm writing up this blog post for anyone working with me, or trying to
interact with me in the open source world, who's curious about these
things. Note that what I'm going to describe may not reflect everyone
who observes Jewish holidays. In particular, I'm referring to the
Orthodox Jewish observation of holidays, and the holiday schedule
within Israel (which is slightly different to outside of Israel).

There's also some really cool math that comes into play, both
sunrise/sunset calculations, and the lunar/solar calendar
synchronization. But I don't intend to get too deep into that (though
feel
[free to read some code about the latter](https://www.stackage.org/package/hebrew-time)).

## The Jewish day

The standard, modern day is something that lasts from one midnight
(12am, or 00:00) to the next. The Jewish day instead lasts from one
sunset to the next. This means when a Jewish holiday falls on Friday,
April 6, 2018, it's _actually_ starting at sundown on Thursday.

An important ramification of this is: __sundown depends completely on
your current location__ (think of timezones). A holiday may start at
6:30pm in my town in northern Israel, at 6:35pm in Haifa, at 8pm in
London, and so on.

As a more advanced note: there's a slight ambiguity about the
definition of sundown, whether it's when the sun dips below the
horizon, or the sky goes dark. We're typically strict in both
directions, starting holidays on the earlier mark, and ending them at
the later mark. So a 1 day Jewish holiday typically lasts 25 hours,
not 24.

## The Sabbath

The Sabbath (aka Shabbat or Shabbos, depending on Hebrew
pronunciation) occurs every Saturday. Which, of course, means every
Friday evening at sundown, until Saturday evening at sundown. Orthodox
Jews observe a number of restrictions on Shabbat. There are far too
many to list here, but some examples are:

* No cooking food (it must be prepared in advance)
* No using a computer/cell phone
* No sewing (a big problem for me)

That may explain to some readers why there's a 25+ hour gap in my
communications every week.

Generally speaking, we refer to the restrictions as "no work," but
that can be a bit misleading. Some things that may be considered
work&mdash;like moving heavy furniture around a house&mdash;aren't
strictly prohibited. And some things that seems to not be
work&mdash;like driving to the beach&mdash;are not allowed. Without
learning (and living!) the detailed rules, it can be all but
impossible to guess what is and isn't allowed on the Sabbath/Shabbat.

## Jewish calendar

The Jewish calendar (aka Hebrew calendar) is a lunar calendar: months
correspond to the cycles of the moon. The first of every Jewish month
(known as Rosh Chodesh) falls on or very close to a new moon, and the
15th falls on or near a full moon. Each Jewish month is either 29 or
30 days long.

However, in addition to being a lunar calendar, the Jewish calendar
synchronizes with the solar calendar. This is necessary, because we
have a requirement that one of the Jewish
holidays&mdash;Passover&mdash;fall out in Spring. There are on average
~354 days in a lunar 12 month cycles, and ~365 days in a complete
solar cycle. Therefore, every 7 out of 19 years we add a leap month.

Advanced note: we also have two months of variable length (Cheshvan
and Kislev, somewhere around October-December) to make all of the math
work out nicely.

Putting this all together: the Jewish calendar syncs up closely with the
Gregorian/solar calendar, but not perfectly. Therefore, holidays will
appear to move around. Technically, the holidays are always on the
same day, just on a different calendar. And due to the solar
synchronization, they stay in the same general season.

## Jewish holidays

There are a number of holidays and special days that fall throughout
the Hebrew calendar. Some of them have the status of a "holy day", and
have almost all the same restrictions as the Sabbath. Others are lower
levels of holiness, and have less restrictions. Without getting into
all of the nitty-gritty details, I'll break things up into 3
categories:

### Holidays where "work" is prohibited

These holidays follow almost all the rules of the Sabbath:

* Rosh Hashana (new year): Tishrei 1 and 2, usually falls out around
  September
* Yom Kippur (day of atonement): Tishrei 10 (8 days later). Also a
  fast day (no eating or drinking)
* First day of Sukkot (tabernacles): Tishrei 15 (5 days later)
* Shmini Atzeret (eighth day of pausing... bad translation): Tishrei
  22 (7 days later)
* First day of Pesach (Passover): Nissan 15, usually falls out around
  April
* Seventh day of Pesach: Nissan 21, 6 days later
* Shavuot (feast of weeks): Sivan 6, usually falls out around May
  (it's always 49 days after the first day of Pesach)

One important thing to note: these holidays basically cluster into two
parts of the year: September/October, also known as the high holiday
season, and Spring for Pesach and Shavuot. So if it seems like I
suddenly am unavailable a lot at the turn of the season: that's why.

Advanced note: outside of Israel, most of these holidays change from
1 day to 2 days.  Both Rosh Hashana and Yom Kippur are exceptions: the
former is 2 days everywhere, the latter 1 day everywhere.

### Holidays with reduced work

These days don't have the same level of prohibition on them for doing
work, but it's generally considered a good thing to do less work.

* Chol hamoed (intermediate days) Sukkot: Tishrei 16-21
* Purim: Adar 14 (or 15 in Jerusalem)
* Chol hamoed Pesach: Nissan 16-20

While possibly the most famous of Jewish holidays, Chanukah (Kislev
25-Tevet 2 or 3) is actually very unrestricted.

### Fast days

There are 6 fast days in the Jewish calendar. Four of them are 12 hour
fasts, from sunrise to sunset, and involve no eating or drinking. They
are:

* Fast of Gedalliah: Tishrei 3
* Fast of Tevet: Tevet 10, around January
* Fast of Esther: Adar 13 (day before Purim)
* Fast of Tammuz: Tammuz 17, around July

There are two other more serious fasts. One was mentioned above (Yom
Kippur). The other is Tisha B'av (Av 9, around August). Both are 24
hour fasts (sundown to sundown), and involve additional restrictions
(like not wearing leather shoes). As mentioned: Yom Kippur is fully
forbidden for "work" like the Sabbath. Tisha B'av is a day of mourning
on the Jewish calendar, and while electronics and the like are
allowed, we are not supposed to do activities that distract us from
mourning, especially in the first half of the day.

## Special restrictions

In addition to the rules above, some special restrictions apply to
some holidays.

On Pesach (Passover), we are not allowed to eat any leavened
items. This basically includes any grain products, though that's a
massive oversimplification. Due to how we execute this restriction,
there's a lot of work that has to be done in advance of Passover,
which can be simplified to "the biggest spring cleaning you've ever
seen."

On Sukkot (Tabernacles), we're required to eat "meals" in a temporary
structure known as a Sukkah, which is sort of like a tent. This
doesn't usually affect my work too much, but can make it all but
impossible to travel on these days.

Note that these restrictions apply to both the three "holy days" of
the holidays listed above (first and seventh days of Pesach, and first
day of Sukkot), as well as the intermediate days. Said another way:
these restrictions each apply for a total of 7 days (or 8 days outside
of Israel).

## Advanced: why the Israel/diaspora difference?

Back in the times of the Jewish temple, we didn't have a fixed
calendar. Instead, new months would be declared by the high court in
Jerusalem (the Sanhedrin) based on eye witness testimony of the new
moon. As a result, we couldn't know from one month to the next for
certain whether the previous month would end up being 29 or 30 days
(though we probably had a good idea based on lunar calculations).

If you notice, Rosh Hashana falls on the 1st and 2nd of the month. The
funny thing is: no one would know on that first day if that day was
going to be Tishrei 1 (because the previous month was 29 days) or the
30th day of the previous month of Elul, because no witness testimony
came forward. If no witnesses came, then in Jerusalem they would need
to celebrate the following day as the real Tishrei 1. If witnesses did
come, they wouldn't need to.

However, there was no way to notify people outside of Jerusalem
whether the previous month turned out to be 29 or 30 days. So everyone
else needed to _always_ observe 2 days to be safe.

For all of the other holidays: the high court would send out
messengers to notify everyone of the length of the previous month. The
messengers would have time to reach all of the land of Israel (it's a
really small place), and therefore within Israel, there was no doubt
about what date it was. But outside of Israel, in many places, the
messengers wouldn't arrive in time. Therefore, they needed to observe
2 days instead. The one exception to this rule was Yom Kippur, since
it wasn't feasible to make people fast for 48 hours straight, and
therefore since the month of Elul had historically always been 29
days, they could assume that had been the case.

Some time later in Jewish history, we switched over to a calendar
system that didn't rely upon eye witness testimony. However, we
continued the custom of observing two days outside of Israel.

You may be brimming with questions about this, like:

* Why continue observing something doubtful, when there's no doubt?
* What about places like Egypt and Jordan that were close enough for
  messengers to arrive?
* What about the holiday of Shavuot (Feast of Weeks), where there was
  more than enough time for messengers to arrive?

These and many other topics can be found in your local Talmudic study
session :).

Final side note: there was a time when we would announce the new month
by lighting fires on mountain tops. It's just like the "beacons of
Gondor" seen in "The Return of the King."
