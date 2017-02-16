Better exception messages
=========================

Let's write a really silly, highly inefficient (my favorite kind!) program that
connects to multiple HTTP servers and sends a very simple request. Using the
network package, this is really straightforward:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-8.0 runghc --package network -- -Wall -Werror
import Control.Monad (forM, forM_)
import Network       (PortID (PortNumber), PortNumber, connectTo)
import System.IO     (hClose, hPutStrLn)

dests :: [(String, PortNumber)]
dests =
    [ ("localhost", 80)
    , ("localhost", 8080)
    , ("10.0.0.138", 80)
    ]

main :: IO ()
main = do
    handles <- forM dests $ \(host, port) -> connectTo host (PortNumber port)
    forM_ handles $ \h -> hPutStrLn h "GET / HTTP/1.1\r\n\r\n"
    forM_ handles hClose
```

We have our destinations. We open a connection to each of them, send our data,
and then close the connection. You may have plenty of objections to how I've
written this: we shouldn't be using `String`, shouldn't we flush the `Handle`,
etc. Just ignore that for now. I'm going to run this on my local system, and
get the following output:

```
$ ./foo.hs 
foo.hs: connect: does not exist (Connection refused)
```

Riddle me this: which of the destinations above did the connection fail for?
Answer: without changing our program, we have no idea. And that's the point of
this blog post: all too often in the Haskell world, we get error messages from
a program without nearly enough information to debug it. `Prelude.undefined`,
`Prelude.read: no parse`, and `Prelude.head: empty list` are all infamous
examples where a nice stack trace would save lots of pain. I'm talking about
something slightly different.

When you throw an exception in your code, whether it be via `throwIO`,
returning `Left`, using `fail`, or using `error`, please _give us some
context_. During development, it's a pain to have to dive into the code, add
some trace statements, figure out what the actual problem is, and then remove
the trace statements. When running in production, that extra information can be
the difference between a two-minutes operations level fix (like opening a port
in the firewall) versus a multi-hour debugging excursion.

Concretely, here's an example of how I'd recommend collecting more information
from `connectTo`:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc --resolver lts-5.10 runghc --package network -- -Wall -Werror
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Exception (Exception, IOException, catch, throwIO)
import Control.Monad     (forM, forM_)
import Data.Typeable     (Typeable)
import Network           (HostName, PortID (PortNumber), PortNumber, connectTo)
import System.IO         (Handle, hClose, hPutStrLn)

data ConnectException = ConnectException HostName PortID IOException
    deriving (Show, Typeable)
instance Exception ConnectException

connectTo' :: HostName -> PortID -> IO Handle
connectTo' host port = connectTo host port `catch`
    \e -> throwIO (ConnectException host port e)

dests :: [(String, PortNumber)]
dests =
    [ ("localhost", 80)
    , ("localhost", 8080)
    , ("10.0.0.138", 80)
    ]

main :: IO ()
main = do
    handles <- forM dests $ \(host, port) -> connectTo' host (PortNumber port)
    forM_ handles $ \h -> hPutStrLn h "GET / HTTP/1.1\r\n\r\n"
    forM_ handles hClose
```

Notice how the `ConnectException` datatype provides plenty of information about
the context that `connectTo'` was called from (in fact, _all_ available
information). If I run this program, the problem is immediately obvious:

```
$ ./bar.hs 
bar.hs: ConnectException "localhost" (PortNumber 80) connect: does not exist (Connection refused)
```

My web server isn't running locally on port 80. My ops team can now go kick the
nginx/Warp process or do whatever other magic they need to do to get things
running. All without bothering me at 2am :)

You may be thinking that this extra data type declaration is a lot of
boilerplate overhead. While it does add some tedium, the benefit of being able
to not only catch the exact exception we care about, but also easily extract
the relevant context information, can pay off in completely unexpected ways in
the future. I highly recommend it.

Since no Haskell blog post about exceptions is complete without it, let me
cover some controversy:

* I know some people absolutely hate runtime exceptions. This point is
  orthogonal: however you decide to report exceptions to your users (`Left`,
  `ExceptT`, impure exceptions, etc), be kind to them and provide this extra
  context information.
* There are some problems with the approach I gave above regarding hierarchical
  exceptions. I'm specifically _not_ diving into the details of hierarchical
  exceptions right now, since it's a complex topic that deserves its own
  dedicated post.
* Similar to the above point, it's a fair question whether you should group all
  exceptions together in one type with lots of data constructors for a
  package/module, or create lots of separate datatypes. Again, proper design of
  an exception type really deserves its own post. FWIW, in http-client, I
  elected to have an `HttpException` type with lots of data constructors.

Also, I left it out for brevity, but including a `displayException` method in
your `Exception` instance can allow programs to display much more user-friendly
error messages to end users.

While nothing I've said here is revolutionary, it's a small tweak to a library
author's development style that can have a profound impact on users of the
library, both at the dev level and those running the executable itself.
