hspec-expectations-lens
======
[![Hackage](https://budueba.com/hackage/hspec-expectations-lens)](http://hackage.haskell.org/package/hspec-expectations-lens)
[![Build Status](https://secure.travis-ci.org/supki/hspec-expectations-lens.png?branch=master)](http://travis-ci.org/supki/hspec-expectations-lens)

This is an literate haskell file, so we start with pragmas,

> {-# LANGUAGE ExtendedDefaultRules #-}

module declaration,

> module Main (main) where

and imports we'll need later.

> import Control.Lens
> import Control.Exception (evaluate)
> import Control.Exception.Lens (_ErrorCall, _DivideByZero)
> import Test.Hspec.Lens

> main :: IO ()
> main = hspec $ do

The format of this readme is a working [hspec][hspec] spec. You can run it using `runhaskell`:

    % runhaskell README.lhs

[hspec-expectations-lens][hspec-expectations-lens] expectations can be divided into several groups.
The first one contains [`shouldHave`][shouldHave] and [`shouldNotHave`][shouldNotHave] combinators; they only
check whether a [`Fold`][Fold] you give them has (or has not) any targets in the structure.

>   describe "shouldHave" $ do
>     it "checks 'Fold' has targets" $
>       Right (Just (Left 'a')) `shouldHave` _Right

We can look multiple levels deep into structure; for instance, [`_Right`][Right]`.`[`_Just`][Just]`.`[`_Left`][Left] would also work here:

>     it "looks as deep into structure as you want" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just

Finally, `shouldHave` can happily be used as a replacement for a vanilla [`shouldBe`][shouldBe].

>     it "replaces 'shouldBe'" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just._Left.only 'a'

>     it "can check for equality a bigger part of the structure" $
>       Right (Just (Left 'a')) `shouldHave` _Right._Just.only (Left 'a')

>     it "can check for equality the whole structure" $
>       Right (Just (Left 'a')) `shouldHave` only (Right (Just (Left 'a')))

`shouldNotHave` is similar to `shouldHave` but it's exactly the opposite thing: it checks
whether the `Fold` does not have any targets in the structure.

>   describe "shouldNotHave" $
>     it "is the opposite of 'shouldHave'" $
>       Right (Just (Left 'a')) `shouldNotHave` _Left

Next group of combinators is those of them that actually care about the result of looking into the structure.
It consists of [`shouldView`][shouldView], [`shouldPreview`][shouldPreview], and [`shouldList`][shouldList].
They derive names from [`view`][view], [`preview`][preview], and [`toListOf`][toListOf] combinators
from the [`lens`][lens] package.

`shouldView` works similarly to `view`, no surprises here. If a `Fold` has one target, you just get it:

>   describe "shouldView" $ do
>     it "gets single target verbatim" $
>       (1, (((1, 7), 8), 2, (3, 4), 5, 7)) `shouldView` 7 `through` _2._1._1._2

([`through`][through] is a fancy name for `id` to avoid parentheses here and there)

Otherwise, if `Fold` has multiple targets, they are combined using `Monoid` instance for the type:

>     it "combines multiple targets using Monoid instance" $
>       [("foo", 1), ("bar", 2), ("baz", 3)] `shouldView` "foobarbaz" `through` folded._1

`shouldList` is like `toListOf`, that is, it combines targets into list:

>   describe "shouldList" $
>     it "combines multiple targets into list" $
>       [("foo", 1), ("bar", 2), ("baz", 3)] `shouldList` [1, 2, 3] `through` folded._2

`shouldPreview` is like `preview` in that it only cares about the first target:

>   describe "shouldPreview" $
>     it "gets the first target" $
>       [1..10] `shouldPreview` 10 `through` reversed.folded

The last combinator, which does not really belong to any of the previous groups,
is [`shouldThrow`][shouldThrow]. It uses composable selectors to determine which exception was thrown:

>   describe "shouldThrow" $ do
>     it "can look into the 'error' call" $
>       error "foo" `shouldThrow` _ErrorCall.only "foo"

>     it "can look into the division by zero" $
>       evaluate (1 `div` 0) `shouldThrow` _DivideByZero

  [hspec]: http://hspec.github.io/
  [lens]: https://github.com/ekmett/lens/
  [hspec-expectations-lens]: http://supki.github.io/hspec-expectations-lens/
  [Fold]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Type.html#t:Fold
  [Right]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Prism.html#v:_Right
  [Just]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Prism.html#v:_Just
  [Left]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Prism.html#v:_Left
  [view]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Getter.html#v:view
  [preview]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Fold.html#v:preview
  [toListOf]: http://hackage.haskell.org/package/lens-3.10.1/docs/Control-Lens-Fold.html#v:toListOf
  [shouldBe]: http://hackage.haskell.org/package/hspec-expectations-0.5.0.1/docs/Test-Hspec-Expectations.html#v:shouldBe
  [shouldHave]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldHave
  [shouldNotHave]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldNotHave
  [shouldView]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldView
  [shouldPreview]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldPreview
  [shouldList]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldList
  [shouldThrow]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:shouldThrow
  [through]: http://supki.github.io/hspec-expectations-lens/Test-Hspec-Expectations-Lens.html#v:through
