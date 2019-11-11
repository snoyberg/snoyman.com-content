---
title: Your first Haskell web app with WAI and Warp
---

## Your first Haskell web app with WAI and Warp

* Michael Snoyman
* VP Engineering, FP Complete<br><img alt="FP Complete logo" src="https://tech.fpcomplete.com/images/fp-complete-logo-small.png" style="border:0">
* Functional Conf 2019
* Friday, November 15, 2019

---

## Why WAI?

* Web Application Interface - pronounced "why"
* I wanted to do web development in Haskell
* Didn't want to have to maintain my own web server (hah!)
* Interface shared among multiple frameworks
* Swap out backends: real server, testing, CGI (for fellow dinosaurs)
* Middlewares (gzip, logging, etc)

---

## Hello WAI!

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-14.10 script
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

main :: IO ()
main = run 3000 $ \_req send ->
  send $ responseBuilder
    status200
    [("Content-Type", "text/plain; charset=utf-8")]
    "Hello World from WAI!"
```

We'll discuss all of this (and more!) in this talk

---

## Goals

* Minimal overhead
* Unopinionated
* Extensible
* Stable
* Batteries not included, but available

---

## Common packages

* `wai`: core data types, a few utilities
* `warp`: de facto standard server
* `wai-extra`: common middlewares and helpers
* `wai-conduit`: conduit-specific streaming support
* `pipes-wai`: pipes-specific streaming support
* `wai-websockets`: you can probably guess :)

---

## Data types

```haskell
-- Given to you by the backend
data Request

-- Smart constructors, we'll show 'em later
data Response

-- Doesn't actually exist
type SimpleApp = Request -> IO Response

-- More complicated, we'll get to it
type Application = ...

type Middleware = Application -> Application
```

---

## Minimal parsing

`Request` has fields like:

```haskell
data Request = Request
  { rawPathInfo    :: ByteString
  , rawQueryString :: ByteString
  , pathInfo       :: [Text]
  , queryString    :: [(ByteString, Maybe ByteString)]
  , ...
  }
```

Covers GET parameters, but how about post parameters?

```haskell
getRequestBodyChunk :: Request -> IO ByteString
```

And use `wai-extra` to actually parse the body

---

## Response smart constructors

Use a lazy ByteString

```haskell
responseLBS
  :: Status
  -> [ResponseHeader]
  -> LazyByteString
  -> Response
```

Or better: use a Builder

```haskell
responseBuilder
  :: Status
  -> [ResponseHeader]
  -> Builder
  -> Response
```

Efficient buffer filling, can reduce memory copying and system call overhead!

---

## Response from a file

* Some backends (like Warp) can use the `sendfile` system call
* Bypasses buffer copies/system calls
* Other backends may need to fall back to `read`ing the file

```haskell
responseFile
  :: Status
  -> [ResponseHeader]
  -> FilePath
  -> Maybe FilePart -- send a few pieces
  -> Response
```

Yesod, `wai-app-static`, others call this for you

---

## Streaming and `Application`

* Just want a `Request -> IO Response`
* However, need to handle streaming data cases
* Example warranted (most complicated thing today)

```haskell
type SimpleApp = Request -> IO Response
simpleRun :: Int -> SimpleApp -> IO ()

main =
  simpleRun 8000 $ \_req ->
  withBinaryFile "big-file.csv" ReadMode $ \h -> do
    lbs <- BL.hGetContents h -- lazy read!
    pure $ responseLBS
      status200
      [("Content-Type", "text/csv; charset=utf-8")]
      lbs
```

Who can find the bug?

---

## Extend the bracket!

* Need to run `withBinaryFile` outside of the response sending
* So we provide a `send` callback

```haskell
main :: IO ()
main =
  run 8000 $ \_req send ->
  withBinaryFile "big-file.csv" ReadMode $ \h -> do
    lbs <- BL.hGetContents h
    send $ responseLBS
      status200
      [("Content-Type", "text/csv; charset=utf-8")]
      lbs
```

Let's compare side by side

---

## Did you miss it?

### Bad

```haskell
withBinaryFile "big-file.csv" ReadMode $ \h -> do
  lbs <- BL.hGetContents h -- lazy read!
  pure $ responseLBS
    status200
    [("Content-Type", "text/csv; charset=utf-8")]
    lbs
```

### Good

```haskell
withBinaryFile "big-file.csv" ReadMode $ \h -> do
  lbs <- BL.hGetContents h -- still lazy, but OK
  send $ responseLBS
    status200
    [("Content-Type", "text/csv; charset=utf-8")]
    lbs
```

---

## ResponseReceived trick

Want `send` to be called exactly once

```haskell
-- Data constructor only exported in internal module
data ResponseReceived = ResponseReceived

type Send = Response -> IO ResponseReceived
type Application = Request -> Send -> IO ResponseReceived
```

* Warp and other backends use the internal module
* Normal apps _must_ call send to get a `ResponseReceived` value
* They can still cheat and call it twice... we don't have linear types

---

## Continuous Passing Style

<img src="/static/yo-dawg-cps.jpeg">

_Done with the hard part!_

---

## Routing

Use `pathInfo`, handles splitting, char encoding, etc.

```haskell
main = do
  run 8000 $ \req send -> do
    let okHelper = send . responseBuilder status200 []
    case pathInfo req of
      [] -> okHelper "Home page"
      ["foo"] -> okHelper "/foo"
      ["foo", "bar"] -> okHelper "/foo/bar"
      _ -> send $ responseBuilder status404 [] "Not found"
```

* Can route on query string parameters too
* Need to parse the request body to get post parameters

---

## Logging

Hello Middleware!

```haskell
hello :: Application
hello _req send =
  send $ responseBuilder status200 [] "Hello!"

loggedHello :: Application
loggedHello = logStdout hello

main :: IO ()
main = run 8000 loggedHello
```

Lots of additional options for output dest, display, etc

---

## Write our own middleware

```haskell
chaos :: Middleware
chaos app req send = do
  let newReq = req { pathInfo = "marauder" : pathInfo req }
  putStrLn "I am up to no good"
  output <- app newReq send
  putStrLn "Mischief managed"
  pure output

main :: IO ()
main = run 8000 $ chaos loggedHello
```

* Perform actions before and after app
* Modify information app receives
* Can be layered with other middleware

---

## Virtual hosts (1)

Can make decisions based on headers as well

```haskell
main :: IO ()
main = run 8000 $ \req send ->
  send $ responseBuilder status200 [] $
  case lookup "host" $ requestHeaders req of
    Nothing -> "No host header found"
    Just host -> "Host is " <> byteString host
```

Or even better: different apps...

---

## Virtual hosts (2)

Serve different apps per domain

```haskell
main = run 8000 $ \req send ->
  case lookup "host" $ requestHeaders req of
    Just "www.example.com" -> app1 req send
    Just "www.example.org" -> app2 req send
    Nothing ->
      send $ responseBuilder status400 [] "No host"
    Just host ->
      send $ responseBuilder status400 [] $
      "Unknown host: " <> byteString host
```

Pass in the `req` and `send` to the sub-apps

---

## Ready-to-go apps

Want a static file server?

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-14.10 script
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Application.Static

main :: IO ()
main = run 8000 $ logStdout $ staticApp $
       defaultFileServerSettings "content"
```

* And we got logging
* Check out `wai-middleware-auth` to secure it
* Can combine with a Yesod app using virtual hosts

---

## Deployment overview

* Commonly use Warp as backend
* Reverse proxy from nginx, Kubernetes, AWS load balancer
* Can listen on user-facing port as well
* `warp-tls` provides pure-Haskell secure deployment option
* Can even go retro and use CGI
* Haskell apps compile to machine executable, copy to machine
* Or use something like Docker to package it all up

---

## Deployment example caveat

* Don't do what I'm about to show you
* No good reason to package up multiple apps like this
* Kubernetes can handle the load balancing better

<img src="https://www.fpcomplete.com/static/hysterical-raisins.jpg" height="300">

---

## Deployment example - snoyman-webapps

* Two webapps (snoyman.com and yesodweb.com)
* Third app as reverse proxy in front of them
    * Uses `http-reverse-proxy`
* Reverse proxy app launches and keeps other two running
* Gitlab CI builds all three, packages into a Docker image

---

## build-docker.sh

```shell
mkdir -p \
  docker/artifacts/app/yesodweb.com \
  docker/artifacts/app/webapps \
  docker/artifacts/bin
stack install --local-bin-path docker/artifacts/bin
cp -r sites/yesodweb.com/config sites/yesodweb.com/static \
  docker/artifacts/app/yesodweb.com
cp -r webapps/config docker/artifacts/app/webapps
docker build --tag snoyberg/snoyman-webapps docker
```

---

## Dockerfile

```
FROM fpco/pid1:18.04

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y git && \
    apt-get clean && \
    unset DEBIAN_FRONTEND

COPY artifacts/bin /usr/local/bin
COPY artifacts/app/ /app
```

* Uses `git` at runtime
* `fpco/pid1` provides zombie prevention and more

---

## .gitlab-ci.yml

```yaml
build:
  stage: build
  script:
    - docker/build-docker.sh
    - docker tag snoyberg/snoyman-webapps "${IMGNAME}"

deploy:
  stage: deploy
  only:
    - master
  script:
    - kubectl set image "$KUBENAME" webapps="$IMGNAME"
    - kubectl rollout status "$KUBENAME"
```

---

## Exception handling

* Warp has a fallback exception handler
* Don't rely on it
* Applications should _not_ throw exceptions
    * Middlewares will break!
    * Yesod does rigorous exception catching for you
* Catch exceptions, log them, return error pages
* Be [async exception safe](https://tech.fpcomplete.com/blog/2018/04/async-exception-handling-haskell)

---

## Demo: JSON service

* Keep a map of name/age pairs
* Get list of all names
* Query individuals
* Two ways to add values:
    * PUT with query string to set age
    * POST with request body

[Full code available on Github](https://gist.github.com/snoyberg/6172d8da3fe145d9c80aaa7d495b74e3)

---

## API overview

* `GET /people`
    * JSON array of names
* `POST /people`
    * URL encoded body, requires `name` and `age`
* `GET /person/<name>`
    * `{"name":"<name>","age":<age>}`
* `PUT /person/<name>?age=<age>`

---

## Type synonyms

```haskell
-- Type synonyms for nicer signatures below
type Name = Text
type Age = Int
type PeopleMap = Map Name Age
type PeopleVar = TVar PeopleMap
```

---

## Response helpers

```haskell
-- Common error responses
notFound :: Response
notFound = responseBuilder status404 [] "Not found"

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad req method"

-- | Build a successful JSON response
jsonResponse :: ToJSON a => a -> Response
jsonResponse = responseBuilder
  status200
  [(hContentType, "application/json")]
  . fromEncoding . toEncoding
```

---

## Router

```haskell
peopleApp :: PeopleVar -> Application
peopleApp peopleVar req send = do
  response <-
    case pathInfo req of
      ["people"] ->
        case requestMethod req of
          "GET" -> getPeopleResponse peopleVar
          "POST" -> postPeopleResponse peopleVar req
          _ -> pure badRequest
```

```haskell
      ["person", name] ->
        case requestMethod req of
          "GET" -> getPersonResponse peopleVar name
          "PUT" -> do
            let ageParam = lookup "age" $ queryString req
            putPersonResponse peopleVar name ageParam
      _ -> pure notFound
  send response
```

---

## Getters

```haskell
getPeopleResponse :: PeopleVar -> IO Response
getPeopleResponse peopleVar = do
  people <- atomically $ readTVar peopleVar
  pure $ jsonResponse $ Map.keys people

getPersonResponse :: PeopleVar -> Name -> IO Response
getPersonResponse peopleVar name = do
  people <- atomically $ readTVar peopleVar
  case Map.lookup name people of
    Nothing -> pure notFound
    Just age -> pure $ jsonResponse $ object
      [ "name" .= name
      , "age" .= age
      ]
```

---

## Set via PUT

```haskell
putPersonResponse
  :: PeopleVar -> Name -> Maybe (Maybe ByteString)
  -> IO Response
putPersonResponse _ _ Nothing =
  pure $ responseBuilder status400 [] "No age parameter"
putPersonResponse _ _ (Just Nothing) =
  pure $ responseBuilder status400 [] "Empty age parameter"
```

```haskell
putPersonResponse peopleVar name (Just (Just bs)) =
  case Lex.readDecimal bs of
    Just (age, "") -> do
      atomically $ modifyTVar' peopleVar
                 $ Map.insert name age
      pure $ responseBuilder status201 [] ""
    _ -> pure $ responseBuilder status400 []
                "Invalid age parameter"
```

---

## Set via POST

```haskell
postPeopleResponse :: PeopleVar -> Request -> IO Response
postPeopleResponse peopleVar req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  let mpair = do
        nameBS <- lookup "name" params
        name <- either (const Nothing) Just $
                decodeUtf8' nameBS
        ageBS <- lookup "age" params
        (age, "") <- Lex.readDecimal ageBS
        Just (name, age)
```

```haskell
  case mpair of
    Just (name, age) -> do
      atomically $ modifyTVar' peopleVar
                 $ Map.insert name age
      pure $ responseBuilder status201 [] ""
    Nothing -> pure $ responseBuilder status400 []
                      "Invalid parameters"
```

---

## Main function

```haskell
main :: IO ()
main = do
  peopleVar <- newTVarIO mempty
  run 8000 $ logStdout $ autohead $ peopleApp peopleVar
```

Check out the test script in the Gist above

---

## Takeaways

* Perfectly doable to write an app directly with WAI
* Lots of manual plumbing
* Web frameworks handle these things for you

---

## Summary

* WAI is a low level interface
* Basis for many frameworks and some apps
* Lots of common utilities
* Easy to plumb together lots of things
* Probably not the interface you'll use on a daily basis
* But totally usable if you need it

Questions? Thank you!
