{-# LANGUAGE Rank2Types #-}
-- | Hspec expectations for the lens stuff
module Test.Hspec.Expectations.Lens
  ( -- * Expectations
    shouldHave, shouldNotHave
  , shouldView
  , shouldPreview
  , shouldList
  , through
  ) where

import Control.Applicative (Const(..))
import Data.Monoid (Any(..), First(..), Endo(..))
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (assertBool)

{-# ANN module "HLint: Use camelCase" #-}


infixl 1 `shouldHave`, `shouldNotHave`, `shouldView`, `shouldPreview`, `shouldList`, `through`

-- | @x \`shouldHave\` l@ sets the expectation that 'Fold' @l@ has
-- non-zero number of targets in @x@
shouldHave
  :: Show s
  => s
  -> ((a -> Const Any a) -> s -> Const Any s)
  -> Expectation
x `shouldHave` f = assertBool errorMsg (has f x)
  where
    errorMsg = unwords ["Supplied Fold has zero targets for", show x]

-- | @x \`shouldNotHave\` l@ sets the expectation that 'Fold' @l@
-- has zero targets in @x@
shouldNotHave
  :: Show s
  => s
  -> ((a -> Const Any a) -> s -> Const Any s)
  -> Expectation
x `shouldNotHave` f = assertBool errorMsg (hasn't f x)
  where
    errorMsg = unwords ["Supplied Fold has targets for", show x]

-- | @x \`shouldView\` y \`through\` l@ sets the expectation that
-- you can see @y@ in @x@ though a 'Getter' @l@
shouldView
  :: (Show s, Show a, Eq a)
  => s
  -> a
  -> ((a -> Const a a) -> s -> Const a s)
  -> Expectation
(x `shouldView` y) l = assertBool errorMsg (view l x == y)
  where
    errorMsg = unwords ["Can't view", show y, "from", show x, "through supplied Getter"]

-- | @x \`shouldPreview\` y \`through\` l@ sets the expectation that
-- you can list @y@ in @x@ first though a 'Fold' @l@
shouldPreview
  :: (Show s, Show a, Eq a)
  => s
  -> a
  -> ((a -> Const (First a) a) -> s -> Const (First a) s)
  -> Expectation
(x `shouldPreview` y) l = assertBool errorMsg (preview l x == Just y)
  where
    errorMsg = unwords ["Can't preview", show y, "from", show x, "through supplied Fold"]

-- | @x \`shouldList\` ys \`through\` l@ sets the expectation that
-- you can list @ys@ in @x@ though a 'Fold' @l@
shouldList
  :: (Show s, Show a, Eq a)
  => s
  -> [a]
  -> ((a -> Const (Endo [a]) a) -> s -> Const (Endo [a]) s)
  -> Expectation
(x `shouldList` y) l = assertBool errorMsg (toListOf l x == y)
  where
    errorMsg = unwords ["Can't list", show y, "from", show x, "through supplied Fold"]

-- | A helper to fight parentheses
--
-- @
-- through ≡ id
-- @
through :: a -> a
through = id

has :: ((a -> Const Any b) -> s -> Const Any t) -> s -> Bool
has l = getAny . foldMapOf l (\_ -> Any True)

hasn't :: ((a -> Const Any b) -> s -> Const Any t) -> s -> Bool
hasn't l = not . has l

view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l = foldMapOf l id

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview l s = getFirst (foldMapOf l (First . Just) s)

toListOf :: ((a -> Const (Endo [a]) a) -> s -> Const (Endo [a]) s) -> s -> [a]
toListOf l s = appEndo (foldMapOf l (\x -> Endo (x :)) s) []

foldMapOf :: ((a -> Const m b) -> s -> Const n t) -> (a -> m) -> s -> n
foldMapOf l f = getConst . l (Const . f)
