Discuss the different ways to handle exceptions in different languages.
Leads to `ExceptT`
Problems with transformers...

* https://www.snoyman.com/reveal/monad-transformer-state
* https://www.yesodweb.com/blog/2014/05/exceptions-cont-monads

Even ignoring that: `ExceptT` doesn't work thanks to async exceptions

* https://haskell-lang.org/library/safe-exceptions

Best practices therefore

* https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell

Some bonus fun

* https://haskell-lang.org/tutorial/primitive-haskell
* <https://wiki.haskell.org/Evaluation_order_and_state_tokens>

TODO Motivate the `RIO` data type and the `rio` package
