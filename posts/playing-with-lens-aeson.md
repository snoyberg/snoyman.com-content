At LambdaConf last week, Tony Morris convinced me I should take
another stab at getting more comfortable with lens, and after chatting
with a few other people (including at least Chris Allen), I decided
that the
[lens-aeson](https://www.stackage.org/package/lens-aeson)/JSON parsing
use case would be a good at forcing me to play with more of the lens
ecosystem than I have previously.

This is not a normal blog post for me. I'm not an expert (or even
competent) on the topic of lens. In fact, odds are no one should read
this blog post. Really consider it me thinking out loud, and
obnoxiously doing so on my blog. I'll excuse the weird nature of this
by saying I'm running on little sleep, and I'm bored in an airport and
on an airplane.

* * *

Let's start off with a simple JSON file containing color names and
values that looks like this:

```json
[
    {
        "color": "red",
        "value": "#f00"
    },
    {
        "color": "black",
	    "value": "#000"
    }
]
```

This is a relatively simple file format, with an array of individual
objects, and each object having the same keys. We want to get the
names of all the colors from this, ignoring the values. Let's start
off by implementing such a program using an explicit `FromJSON`
instance, which is probably the most obvious thing to do based on the
lens documentation.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString as B

data Color = Color { colorName :: !Text }

instance FromJSON Color where
  parseJSON = withObject "Color" $ \o -> Color <$> o .: "color"

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  case eitherDecodeStrict' bs of
    Left e -> error e
    Right colors -> print $ map colorName colors
```

This is pretty straightforward: we define a data type `Color`, which
contains the fields we care about (here, just the name of the
color). Then we declare a `FromJSON` instance which parses out the
`color` key. In our `main` function, we read the raw bytes, and use
`eitherDecodeStrict'` to parse the JSON into a `Value` and then use
our `FromJSON` instance to convert that `Value` into a list of `Color`
values. We then apply `colorName` to each value in that list to
extract the name, and print the list.

That works, but it's far from inspiring. We're declaring a `Color`
datatype simply for the purpose of writing a typeclass instance. But
it feels pretty heavyweight to have to declare a data type and make a
typeclass instance for just one use site. Let's try what I'd consider
the next most obvious approach: work directly on the `Value` data
type's constructors:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  case eitherDecodeStrict' bs of
    Left e -> error e
    Right (Array array) -> do
      colors <- V.forM array $ \v ->
        case v of
          Object o ->
            case HashMap.lookup "color" o of
              Nothing -> error "Didn't find color key"
              Just (String c) -> return c
              Just v' -> error $ "Expected a String, got: " ++ show v'
          _ -> error $ "Expected an object, got: " ++ show v
      print colors
    Right v -> error $ "Unexpected top level type: " ++ show v
```

This works, but is thoroughly unappetizing. We need to take into
account a lot of corner cases and explicitly handle looping over the
`Vector`. It's unpleasant, and for a non-toy example, would be
downright tedious.

Let's try to avoid the tedium, and if you read my intro paragraph, you
won't be surprised to hear that the answer I'm proposing is
`lens-aeson`.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  print $ bs^..values.key "color"._String
```

This code looks almost too short to work, but it produces exactly the
same output as before for our `colors.json` file. To see how it works:

* We don't need to do any explicit parsing of our `ByteString`
  value. `lens-aeson` contains a number of typeclasses for matching
  JSON values, and provides instances for `ByteString`, `Text`, and
  `String` that will perform an initial parse to a `Value` for you
  automatically.
* The `^..` operator comes from the `lens` package, which is a synonym
  for `toListOf`. As you might imagine, it converts _something_ into a
  list. Our `^..` operator will take the value on the left hand side
  (`bs` here) and apply the `Fold` on the right to it, collecting the
  results into a list.
* Now we need to understand how we construct our `Fold`. We start off
  with `values,` which will match a JSON array and provide all of the
  values inside of it.
* Next we compose with the `key "color"` `Fold`, which takes a
  `Value`, checks that it is an `Object`, and looks up the given key,
  in this case `"color"`.
* Finally, we use the `_String` `Fold` to check that we have a string
  value (as opposed to something like a number or a boolean) and
  returns it.

The behavior of this isn't exactly identical to our previous
versions. In particular, if there are values in our array that don't
match our requirements, they'll simply be dropped instead of producing
an error. Whether this is acceptable for your case is up to you. And
I'm hoping that someone reading this post will provide a good example
of how to do the error-checking version with `lens-aeson`.

## Not just a `Fold`

Above, I mentioned the term `Fold` many times. A `Fold` is one kind of
_optic_ from the lens package, which "allows you to extract multiple
results from a container." However, if you're familiar with lens, you
may know that optics form a hierarchy.

__NOTE__ An _optic_ is a more general term that encompasses a lot of
the types in the lens package, like lenses, foldables, prisms,
traversables, isos, getters, etc. Because of how optics are
structured, they compose together nicely. And because of how the
typeclasses are structured, optics have a nice subtyping system, which
I'm hinting at here.

For example, a `Traversal` is a generalization of a `Fold` which also
allows us to "traverse over a structure and change out its contents
with monadic side-effects." Our `values` `Fold` isn't just a
`Fold`. It allows us to also update all of the values inside the
array, making it a valid `Traversal`. Let's see how we can use that:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

main :: IO ()
main = do
  let bs = "[1,2,3]" :: B.ByteString
  print $ bs & values._Number %~ (+ 1)
```

Instead of reading our `ByteString` from a file, we're now defining
our `bs` value in our Haskell code, giving it the JSON representation
of the array of numbers 1, 2, and 3.

We then take our `ByteString` and use the `&` operator, which is
reverse function application. This means that we will apply whatever's
on the right hand side of `&` to our `ByteString` on the left. Let's
look at that function:

```haskell
values._Number %~ (+ 1)
```

The `%~` operator will apply some modification function using a
`Setter`. And guess what: a `Traversal` is a generalization of a
`Setter`, so we can use a `Traversal`. As we said, `values` is a
`Traversal`. `_Number` is also a `Traversal`, so their composition
makes a `Traversal`. And then we apply our `+ 1` function inside of
it.

So to sum up, our `bs & values._Number %~ (+ 1)` expression will do
the following:

* Parse the raw bytestring value in `bs` into a JSON `Value`
* Inspect that value and see if it's an array
* For each element in that array, check if it's a number
* If it's a number, add 1 to it
* Finally, take the newly created `Value` and render it back into a
  bytestring value

That's quite the power-to-weight ratio. I recommend writing the same
thing without lens for comparison.

## Not just a `Traversal`

The same way a `Traversal` is a generalization of a `Fold`, a `Prism`
is a generalization of a `Traversal`. While a `Traversal` represents
the ability to look inside a value, find 0 or more values of a given
type, and either get them (the `Fold` power) or modify them (the
`Traversal` power), a `Prism` specificies that it will have _exactly_
0 or 1 values, and that, given one value of the target type, you
create the original type.

Did that sound confusing? I certainly think so. So let's say it
another way: a `Prism` is an optic version of a data constructor. When
you have a sum type `Either a b`, you can always get exactly 0 or 1
`a` values (0 if the value is `Right`, 1 if the value is `Left`). And,
given an `a` value, you can always construct a value of type `Either a b`.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
import Control.Lens
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "constructs with _Left" $
    (1 ^. re _Left) `shouldBe`
    (Left 1 :: Either Int String)
  it "constructs with _Right" $
    ("hello" ^. re _Right) `shouldBe`
    (Right "hello" :: Either Int String)
  it "traverses with _Left" $
    (Left 1 & _Left %~ (+ 1)) `shouldBe`
    (Left 2 :: Either Int String)
  it "traverse can do nothing" $
    (Right "hello" & _Left %~ (+ 1)) `shouldBe`
    (Right "hello" :: Either Int String)
  it "folds with _Left" $
    (Left 1 ^.. _Left) `shouldBe`
    [1 :: Int]
  it "folds with _Right" $
    (Left 1 ^.. _Right) `shouldBe`
    ([] :: [()])
```

So apparently, if you're totally bought in on the lens ecosystem,
you're free to never use your data constructors again and just use
`re`. But anyway, we were dealing with JSON data; can we construct a
simple JSON value like this? Sure.

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.Vector as V

main :: IO ()
main = putStrLn $ 1 ^. re _Number.to (V.replicate 5).re _Array
```

The `to` function converts a normal functions from `a` to `b` into an
optic that does the same thing, a `Getter a b`. More idiomatically (I
think), we'd actually use the type variables `s` and `a` and get `to
:: (s -> a) -> Getter s a`.

This was actually more detailed on lens itself than I intended to get
here, but since this blog post is just a forcing function for me to
explore things and not actually useful for anyone else in the world, I
guess that's OK.

## More random fun

Alright, can I upper case all of the color names? Sure:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  print $ bs & values.key "color"._String %~ T.toUpper
```

Now let's get a bit trickier: can I create an _additional_ field
`color-upper` with this upper cased version? I have no idea if this is
idiomatic lens code, but it certainly works:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  print $ bs & values._Object %~
    (\hm -> hm & at "color-upper" .~
      (hm^?at "color".folded._String.to T.toUpper.re _String))
```

That's a lot to unpack for me. First, I'm using `bs & values._Object
%~ ...` to say "look inside the bytestring, treat it as JSON, look for
an array, and find every object in that array and treat it as a
`HashMap Text Value`, and modify each hashmap using the ..." It's the
`...` that I find confusing.

Next, we do `hm & at "color-upper" .~ ...`, which says "I want to set
the value in the hashmap at the key `color-upper` to the `Maybe Value`
value I'm giving you. Finally, we get our `Maybe Value` value with the
rest of that expression, which reads:

```haskell
hm^?at "color".folded._String.to T.toUpper.re _String
```

This reads to me as:

* Take `hm`
* Give me the first value that succeeds (`^?`), or `Nothing` if no
  value gets grabbed
* Look up the `"color"` key
* Flatten out that `Maybe Value` into just a `Value`
* Check that it's a string
* Convert it to upper case
* Wrap it back in a `String` constructor using `re _String`

By way of contrast, I can write the same functionality the non-lens way with:

```haskell
\hm ->
  case HashMap.lookup "color" hm of
    Just (String color) -> HashMap.insert
      "color-upper"
      (String (T.toUpper color))
      hm
    _ -> hm
```

For me personally, I find this version easier to read, but I'm also a
lens usage novice. Maybe I just need to force myself to write
airplane-powered rambling lens blog posts more often (or maybe write
some real code).

Going for something much simpler, let's just delete all of the `value`
keys:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  print $ bs & values._Object %~ sans "value"
```

### Indexed

I wanted to play with indexed optics a bit. My goal had been to modify
the following code:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-8.12 script
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

main :: IO ()
main = do
  bs <- B.readFile "colors.json"
  print $ bs ^.. values.key "color"._String
```

So that it printed a pair of the index in the array that the color
appears at, and the color itself. Unfortunately, I couldn't figure out
how to make that work. One thing I got was:

```haskell
main = do
  bs <- B.readFile "colors.json"
  print $ bs ^@.. values
```

But this just keeps the entire object, not the string inside the
`color` key like I wanted. The following is a bit closer, but (1) it
keeps `Nothing` values in the result instead of just removing them
(like a `mapMaybe` would) and (2) doesn't feel idiomatic:

```haskell
main = do
  bs <- B.readFile "colors.json"
  print $ (bs ^@.. values) & each._2 %~ (^? key "color"._String)
```

Then I discovered the `pre` function, which let me do the following
with identical output to the former:

```haskell
main = do
  bs <- B.readFile "colors.json"
  print $ bs ^@.. values.pre (key "color"._String)
```

It does seem like I'm likely missing something obvious to remove drop
the `Nothing` values and remove the `Maybe` wrapping entirely, but
unfortunately I couldn't figure it out.
