{-# LANGUAGE ExtendedDefaultRules #-}
module Test.Hspec.Expectations.LensSpec (spec) where

import Control.Lens
import Control.Exception
import Control.Exception.Lens
import System.IO.Silently
import Test.Hspec hiding (shouldThrow)
import Test.Hspec.Runner

import Test.Hspec.Expectations.Lens


spec :: Spec
spec = do
  describe "shouldHave" $ do
    it "holds for a non-empty Traversal" $
      shouldHold $
        Just (Just 5, 't') `shouldHave` traverse._1.traverse

    it "holds for a non-empty Fold" $
      shouldHold $
        ("hi", [Nothing, Just 5, Nothing]) `shouldHave` _2.folded.folded

    it "holds for a non-empty Prisms" $
      shouldHold $
        Just (Right (Just 5)) `shouldHave` _Just._Right._Just

    it "does not hold for an empty Traversal" $
      shouldNotHold $
        [1..10] `shouldHave` traverse.filtered(> 10)

    it "does not hold for an empty Fold" $
      shouldNotHold $
        [1..10] `shouldHave` folded.filtered(> 10)

    it "does not hold for an empty Prism" $
      shouldNotHold $
        Just (Right (Just 5)) `shouldHave` _Just._Left._Just

  describe "shouldNotHave" $ do
    it "does not hold for a non-empty Traversal" $
      shouldNotHold $
        Just (Just 5, 't') `shouldNotHave` traverse._1.traverse

    it "does not hold for a non-empty Fold" $
      shouldNotHold $
        ("hi", [Nothing, Just 5, Nothing]) `shouldNotHave` _2.folded.folded

    it "does not hold for a non-empty Prism" $
      shouldNotHold $
        Just (Right (Just 5)) `shouldNotHave` _Just._Right._Just

    it "holds for an empty Traversal" $
      shouldHold $
        [1..10] `shouldNotHave` traverse.filtered(> 10)

    it "holds for an empty Fold" $
      shouldHold $
        [1..10] `shouldNotHave` folded.filtered(> 10)

    it "holds for an empty Prism" $
      shouldHold $
        Just (Right (Just 5)) `shouldNotHave` _Just._Left._Just

  describe "shouldView" $ do
    it "views the Lens target" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldView` 3 `through` _2._1._2

    it "views the Getter target" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldView` 3 `through` to snd.to fst.to snd

  describe "shouldPreview" $ do
    it "previews the first Lens target" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldPreview` 3 `through` _2._1._2

    it "previews the first Getter target" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldPreview` 3 `through` to snd.to fst.to snd

    it "previews the first Fold target" $
      shouldHold $
        Just (Right (Just 7)) `shouldPreview` 7 `through` folded._Right.folded

    it "previews the first Traversal target" $
      shouldHold $
        Just (Right (Just 7)) `shouldPreview` 7 `through` traverse._Right.traverse

  describe "shouldList" $ do
    it "lists Lens targets" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldList` [3] `through` _2._1._2

    it "lists Getter targets" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldList` [3] `through` to snd.to fst.to snd

    it "lists Fold targets" $
      shouldHold $
        [[1, 2, 3], [4, 5, 6]] `shouldList` [2, 4, 6] `through` folded.folded.filtered even

    it "lists Traversal targets" $
      shouldHold $
        [[1, 2, 3], [4, 5, 6]] `shouldList` [1, 3, 5] `through` traverse.traverse.filtered odd

  describe "shouldThrow" $ do
    it "catches ErrorCall" $
      shouldHold $
        error "hi" `shouldThrow` _ErrorCall

    it "catches fine-grained ErrorCall" $
      shouldHold $
        error "hi" `shouldThrow` _ErrorCall.only "hi"

    it "does not catch exceptions which selector does not match" $
      shouldNotHold $
        error "bye" `shouldThrow` _ErrorCall.only "hi"

    it "catches DivideByZero" $
      shouldHold $
        evaluate (1 `div` 0) `shouldThrow` _DivideByZero

  describe "shouldPerform" $
    it "previews a first target of the Fold into IO action result" $
      shouldHold $
        (return (Just 4) :: IO (Maybe Int))  `shouldPerform` 4 `through` _Just


shouldResultIn :: Expectation -> String -> IO ()
shouldResultIn expectation result = do
  r <- fmap (last . lines) . capture_ . hspecWith defaultConfig $
    it "" expectation
  r `shouldBe` result

shouldHold :: Expectation -> Expectation
shouldHold = (`shouldResultIn` "1 example, 0 failures")

shouldNotHold :: Expectation -> Expectation
shouldNotHold = (`shouldResultIn` "1 example, 1 failure")
