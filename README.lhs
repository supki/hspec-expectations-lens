hspec-expectations-lens
=======================
[![Hackage](https://budueba.com/hackage/hspec-expectations-lens)](http://hackage.haskell.org/package/hspec-expectations-lens)
[![Build Status](https://secure.travis-ci.org/supki/hspec-expectations-lens.png?branch=master)](http://travis-ci.org/supki/hspec-expectations-lens)

hspec-expectations-lens is a set of expectations for [Hspec][hspec]
with a [lens][lens]-compatible interface

This README is a literate haskell file which contains a Hspec spec.
You can run it with `runhaskell`; result is something akin to:


```shell
% runhaskell README.lhs

shouldHave
  - checks targets exist
  - looks as deep into structure as you want
  - replaces 'shouldBe'
  - checks a bigger part of the structure for equality
  - checks the whole structure for equality

shouldNotHave
  - is the opposite of 'shouldHave'

shouldView
  - gets a single target verbatim
  - combines multiple targets using Monoid instance

shouldList
  - combines multiple targets into list

shouldPreview
  - gets the first target

shouldPerform
  - performs an action and gets the first target

shouldThrow
  - looks into the 'error' call
  - looks into the division by zero
```

Expectations
------------

We start with pragmas, module declaration, and imports we use later.

> {-# LANGUAGE ExtendedDefaultRules #-} -- this is only neccessary because we don't like annotations
>
> module Main (main) where
>
> import Control.Lens
> import Data.List.Lens
> import Control.Exception (evaluate)
> import Control.Exception.Lens (_ErrorCall, _DivideByZero)
> import Test.Hspec.Lens
>
> main :: IO ()
> main = hspec $ do

hspec-expectations-lens expectations form several groups. First one contains `shouldHave`
and `shouldNotHave` combinators—they check whether a `Fold` you give them has (or doesn't have)
any targets in the structure.

>   describe "shouldHave" $ do
>     it "checks targets exist" $
>       Right (Just (Left 'a')) `shouldHave` _Right

We can look arbitrarily deep into the structure; for instance, `_Right._Just._Left`
would also work here:

>     it "looks as deep into structure as you want" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just

Finally, `shouldHave` can happily be used as a replacement for a vanilla `shouldBe`.

>     it "replaces 'shouldBe'" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just._Left.only 'a'
>
>     it "checks a bigger part of the structure for equality" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just.only (Left 'a')
>
>     it "checks the whole structure for equality" $
>       Right (Just (Left 'a')) `shouldHave` only (Right (Just (Left 'a')))

`shouldNotHave` is the opposite of `shouldHave` as its name implies, it checks
whether a `Fold` does not have any targets in the structure.

>   describe "shouldNotHave" $
>     it "is the opposite of 'shouldHave'" $
>       Right (Just (Left 'a')) `shouldNotHave` _Left

Next group of expectations is those of them that actually care about the result
of looking into the structure.  It consists of `shouldView`, `shouldPreview`,
`shouldList`, and `shouldPerform`.  They derive names from `view`, `preview`, `toListOf`,
and `perform` `combinators from the lens package.

`shouldView` works similarly to `view`, no big surprises here. If a `Fold` is actually
a `Getter`, you just get its target:

>   describe "shouldView" $ do
>     it "gets a single target verbatim" $
>       (1, (((1, 7), 8), 2, (3, 4), 5, 7)) `shouldView` 7 `through` _2._1._1._2

(`through` is just a fancy name for `id`; it helps to avoid parentheses)

Otherwise, if `Fold` has multiple targets, they are combined using `Monoid` instance
for the target type (and it better have one):

>     it "combines multiple targets using Monoid instance" $
>       [("foo", 1), ("bar", 2), ("baz", 3)] `shouldView` "foobarbaz" `through` folded._1

`shouldList` is like `toListOf`, that is, it combines its targets into the list:

>   describe "shouldList" $
>     it "combines multiple targets into list" $
>       [("foo", 1), ("bar", 2), ("baz", 3)] `shouldList` [1, 2, 3] `through` folded._2

`shouldPreview` is like `preview` in that it only cares about the first target:

>   describe "shouldPreview" $
>     it "gets the first target" $
>       [1..10] `shouldPreview` 10 `through` reversed.folded

`shouldPerform` performs the action and looks into the first target of the result.

>   describe "shouldPerform" $
>     it "performs an action and gets the first target" $
>       readFile "README.lhs" `shouldPerform` 99 `through` to lines.folded.prefixed ">".to length

The last combinator, which does not really belong to any of the previous groups,
is `shouldThrow`. It uses composable selectors to determine which
exception was thrown:

>   describe "shouldThrow" $ do
>     it "looks into the 'error' call" $
>       error "foo" `shouldThrow` _ErrorCall.only "foo"
>
>     it "looks into the division by zero" $
>       evaluate (1 `div` 0) `shouldThrow` _DivideByZero

  [hspec]: http://hspec.github.io/
  [lens]: https://github.com/ekmett/lens/
