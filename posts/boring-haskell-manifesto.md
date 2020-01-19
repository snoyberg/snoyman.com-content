__Goal__: how to get Haskell into your organization, and how to make your organization more productive and profitable with better engineering.

I wrote this content 1.5 years ago after conversations with people at LambdaConf 2018. I discussed it offline with some people (at least... I think I did). Then I promptly did nothing else with it. After having some conversations with people at Functional Conf 2019, I decided it was time to resurrect and post this.

If the ideas I'm sharing here resonate with you, and you're interested in getting involved with spreading this further, please reach out to me (privately is great, `michael at snoyman dot com`). I have additional, more concrete ideas on how to make some of this a reality, which I'll be sharing with people offline.

Thanks to everyone at Functional Conf for the inspiration and motivation to move ahead with this!

* * *

Haskell is in many ways a revolutionary language. Many languages in widespread use today are incremental changes on previous languages. However, Haskell doesn't fit this model. Concepts like referential transparency, purely functional programming, laziness, and immutability are a stark departure from common programming approaches today.

Most any Haskeller would argue that these radical changes are well justified, and deliver great benefit. At the same time, this inherent culture of making revolutionary changes attracts a certain mindset to the language. As a result, we end up in a situation where Haskell has essentially two distinct (and overlapping) subcultures:

* Explore interesting and revolutionary concepts in computer science, software engineering, and mathematics
* Make better software

Again, these are overlapping subcultures. The entire history of Haskell is cases of esoteric, academic, intellectual, and "useless" concepts becoming an elegant solution to challenging problems. Monads are probably the most prominent example of this, and we're seeing a reality where many other languages are slowly adopting the concept to solve problems in concurrency and error handling.

On the other hand, not every new concept turns into one of these fundamental and useful techniques. And even for those that do: finding the most practical way to leverage the technique is an arduous, time-consuming process.

Exploring these concepts can be fun, rewarding, and—long term—a huge benefit for productivity. Short and medium term, however, this exploration can lead to slower and less reliable results. As a company or project manager assessing Haskell for a project, this uncertainty can thwart the possibility of adopting Haskell.

We're now faced with a situation where Haskell is often eliminated for usage, representing a massive loss for two parties:

* Companies, projects, and managers who could have realized great benefits in productivity, reliability, and performance from the less revolutionary pieces of Haskell. Instead, they're losing this competitive advantage.
* Engineers who would much prefer working in Haskell—even its less revolutionary subset—are unable to do so because of employer fears of choosing it.

We'd like to improve the situation.

## The Boring Haskell Manifesto

Our claim is simple: for many cases of software engineering, a simple, well-defined subset of Haskell's language and ecosystem will deliver large value for a project, while introducing little to no risk compared to alternative options. We call this subset, somewhat tongue-in-cheek, "boring Haskell." Our goal is to:

* Define such a subset
* Provide straightforward documentation, tutorials, cookbooks, and libraries that encourage this subset
* Continue to evolve the subset to optimize its best practices
* Help interested engineers to learn how to use boring Haskell, and get it adopted in their companies

The most concrete step in this direction was creating the [rio library](https://github.com/commercialhaskell/rio#readme), which is intended to capture these principles. If you want to embrace Boring Haskell today, we recommend using that library. The rest of this document discusses what we believe counts as "Boring Haskell," and motivates these choices.

## Power-to-weight ratio

We want to analyze the features of Haskell that we recommend based on its power-to-weight ratio, also known as a cost-benefit analysis. Put more directly: we want to choose features which provide lots of benefits while minimizing costs. Let's give some examples of these two sides:

Power/benefit

* More maintainable code
* Fewer bugs in production
* Higher performance
* Higher productivity

Weight/cost

* Learning curve
* Ongoing cognitive overhead
* Ongoing tweaking
* Slower compile time
* Poor performance

A concrete example of something with a great power to weight ratio are sum types. Sum types are a relatively simple concept to explain. Most people can grok the concept almost immediately. Pattern matching feels natural fairly quickly. And sum types solve large classes of problems people regularly encounter in programming tasks.

## Reducing risk

*This section is draft quality at best, feel free to stop reading here :)*

A recurring theme I hear from people in industry is the risk of adopting Haskell. Let me address some of the common concerns directly:

**If I'm stuck, there's no support available.** Haskell has a number of companies specializing in consulting services who are able to help. I [work for one of them](https://tech.fpcomplete.com/haskell).

**We can't find people to hire.** This is a valid concern; there are fewer total Haskell engineers. However, you've got some great advantages in Haskell:

* There's less competition on the hiring side versus larger languages.
* People who like Haskell are very motivated to find a job with it.
* If you're willing to hire remotely, there's a large pool of candidates.
* Training people to work with Haskell is a real option. (This is an area that I intend to talk about more in the future.)

**I don't want to invest in learning Haskell if we're just going to keep writing Javascript.** Functional programming principles are powerful, and are being adopted into many languages today. The best way to get good at using them is to learn a language like Haskell that forces you to learn FP.

**I'm afraid that I'm going to hit a wall and Haskell won't work anymore.** This isn't a major issue in practice; most common code people will write is absolutely fine in pure Haskell. However:

* If you need to use some other language because of library availability or something similar, you can always connect with other languages. Microservices architectures&mdash;even if I'm not a huge fan&mdash;come into play here.
* If you hit a performance concern, you can usually solve it in Haskell itself by going lower level. However, the FFI in Haskell is really easy to use, so calling out to something like C or Rust is pretty easy.
* If you're having trouble finding a way to solve things in a functional style, you can always drop down to imperative Haskell. Writing in a full-on imperative style in Haskell, such as all code living in `IO`, may not be idiomatic, but it works. As SPJ has said (quote may not be exact), Haskell is the world's finest imperative language.

## Next steps

For now, I'm just sniffing out interest in this general topic. I have some follow up ideas, but I'd rather get feedback on this idea before going further. Be in touch, and stay tuned!
