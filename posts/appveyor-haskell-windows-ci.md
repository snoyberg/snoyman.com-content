__UPDATE August 2018__ As pointed out by Mark Wotton, the instructions below were out of date (see [Sed: A Debugging Story](https://www.fpcomplete.com/blog/2018/06/sed-a-debugging-story) for more information). I've updated the contents below, but for future readers: please see the more up-to-date AppVeyor configuration on [Stack's Github repo](https://github.com/commercialhaskell/stack/blob/stable/doc/appveyor.yml).

I don't think I ever documented this before, so just a quick post to get this
out there. Many of us working on open source Haskell libraries already use
[Travis CI](https://travis-ci.org/) for doing continuous integration builds of
our software. Some time ago they added support for OS X, making it possible to
cover Linux and OS X with multiple configurations on their systems. For any
project with a [stack.yaml
file](https://docs.haskellstack.org/en/stable/yaml_configuration/), this can be
easily achieved using the [Stack recommended Travis
configuration](https://docs.haskellstack.org/en/stable/GUIDE/#travis-with-caching).

Unfortunately, this leaves Windows testing out, which is unfortunate, because Windows is likely to be the most common build to fail. Fortunately, [AppVeyor](https://www.appveyor.com/) provides a similar experience to Travis, but for Windows. In order to get set up, just:

1. Sign in with their web interface and add your Github repo
2. Add an `appveyor.yml` to your project

Here's a simple file I've used on a few projects with succeess:

```yaml
build: off

before_test:
# http://help.appveyor.com/discussions/problems/6312-curl-command-not-found
- set PATH=C:\Program Files\Git\mingw64\bin;%PATH%

- curl -sS -ostack.zip -L --insecure http://www.stackage.org/stack/windows-i386
- 7z x stack.zip stack.exe

clone_folder: "c:\\stack"
environment:
  global:
    STACK_ROOT: "c:\\sr"

    # Override the temp directory to avoid sed escaping issues
    # See https://github.com/haskell/cabal/issues/5386
    TMP: "c:\\tmp"

test_script:
- stack setup > nul
# The ugly echo "" hack is to avoid complaints about 0 being an invalid file
# descriptor
- echo "" | stack --no-terminal test
```

All this does is:

* Downloads the Stack zip file
* Unpacks the stack.exe executable
* Changes the `STACK_ROOT` to deal with Windows long path issues
* Run `stack setup` to get a toolchain
* Run `stack --no-terminal test` to build your package and run the test suites

You're free to modify this in any way you want, e.g., add in `--bench` to build
benchmarks, add `--pedantic` to fail on warnings, etc. If you have more system
library dependencies, you'll need to consult the AppVeyor docs to see how to
install them. And in our use cases for Stack, we found that using the AppVeyor
caching functionality made builds unreliable (due to the large size of the
cache). You may want to experiment with turning it back on, since this setup is
_slow_ (it downloads and installs a full GHC toolchain and builds all library
dependencies each time).
