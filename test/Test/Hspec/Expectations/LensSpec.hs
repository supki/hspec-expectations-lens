{-# LANGUAGE ExtendedDefaultRules #-}
module Test.Hspec.Expectations.LensSpec (spec) where

import Control.Lens
import System.IO.Silently
import Test.Hspec
import Test.Hspec.Runner

import Test.Hspec.Expectations.Lens


spec :: Spec
spec = do
  describe "shouldHave" $ do
    it "succeeds for non-empty Traversals" $
      shouldHold $
        Just (Just 5, 't') `shouldHave` traverse._1.traverse

    it "succeeds for non-empty Folds" $
      shouldHold $
        ("hi", [Nothing, Just 5, Nothing]) `shouldHave` _2.folded.folded

    it "succeeds for non-empty Prisms" $
      shouldHold $
        Just (Right (Just 5)) `shouldHave` _Just._Right._Just

    it "fails for empty Traversals" $
      shouldNotHold $
        [1..10] `shouldHave` traverse.filtered(> 10)

    it "fails for empty Folds" $
      shouldNotHold $
        [1..10] `shouldHave` folded.filtered(> 10)

    it "fails for empty Prisms" $
      shouldNotHold $
        Just (Right (Just 5)) `shouldHave` _Just._Left._Just

  describe "shouldNotHave" $ do
    it "fails for non-empty Traversals" $
      shouldNotHold $
        Just (Just 5, 't') `shouldNotHave` traverse._1.traverse

    it "fails for non-empty Folds" $
      shouldNotHold $
        ("hi", [Nothing, Just 5, Nothing]) `shouldNotHave` _2.folded.folded

    it "fails for non-empty Prisms" $
      shouldNotHold $
        Just (Right (Just 5)) `shouldNotHave` _Just._Right._Just

    it "succeeds for empty Traversals" $
      shouldHold $
        [1..10] `shouldNotHave` traverse.filtered(> 10)

    it "succeeds for empty Folds" $
      shouldHold $
        [1..10] `shouldNotHave` folded.filtered(> 10)

    it "succeeds for empty Prisms" $
      shouldHold $
        Just (Right (Just 5)) `shouldNotHave` _Just._Left._Just

  describe "shouldView" $ do
    it "views structure through a lens" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldView` 3 `through` _2._1._2

    it "views structure through a getter" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldView` 3 `through` to snd.to fst.to snd

  describe "shouldList" $ do
    it "lists structure through a lens" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldList` [3] `through` _2._1._2

    it "lists structure through a getter" $
      shouldHold $
        (1, ((2, 3), 4)) `shouldList` [3] `through` to snd.to fst.to snd

    it "lists structure through a fold" $
      shouldHold $
        [[1, 2, 3], [4, 5, 6]] `shouldList` [2, 4, 6] `through` folded.folded.filtered even

    it "lists structure through a traversal" $
      shouldHold $
        [[1, 2, 3], [4, 5, 6]] `shouldList` [1, 3, 5] `through` traverse.traverse.filtered odd


shouldResultIn :: Expectation -> String -> IO ()
shouldResultIn expectation result = do
  r <- fmap (last . lines) . capture_ . hspecWith defaultConfig $
    it "" expectation
  r `shouldBe` result

shouldHold :: Expectation -> Expectation
shouldHold = (`shouldResultIn` "1 example, 0 failures")

shouldNotHold :: Expectation -> Expectation
shouldNotHold = (`shouldResultIn` "1 example, 1 failure")
