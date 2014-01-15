-- | Hspec expectations for the lens stuff
module Test.Hspec.Expectations.Lens
  ( -- * Expectations
    shouldHave
  , shouldNotHave
  , shouldView
  , shouldPreview
  , shouldList
  , through
  ) where

import Control.Lens
import Data.Monoid (Any(..), All(..), First(..), Endo(..))
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (assertBool)


infixl 1 `shouldHave`, `shouldNotHave`, `shouldView`, `shouldPreview`, `shouldList`, `through`

-- | @x \`shouldHave\` l@ sets the expectation that 'Fold' @l@ has
-- non-zero number of targets in @x@
--
-- @
-- shouldHave :: 'Show' s => s -> 'Getter'     s a -> 'Expectation'
-- shouldHave :: 'Show' s => s -> 'Fold'       s a -> 'Expectation'
-- shouldHave :: 'Show' s => s -> 'Iso''       s a -> 'Expectation'
-- shouldHave :: 'Show' s => s -> 'Lens''      s a -> 'Expectation'
-- shouldHave :: 'Show' s => s -> 'Traversal'' s a -> 'Expectation'
-- shouldHave :: 'Show' s => s -> 'Prism''     s a -> 'Expectation'
-- @
shouldHave :: Show s => s -> Getting Any s a -> Expectation
x `shouldHave` f = assertBool errorMsg (has f x)
 where
  errorMsg = unwords ["Supplied Fold has zero targets for", show x]

-- | @x \`shouldNotHave\` l@ sets the expectation that 'Fold' @l@
-- has zero targets in @x@
--
-- @
-- shouldNotHave :: 'Show' s => s -> 'Getter'     s a -> 'Expectation'
-- shouldNotHave :: 'Show' s => s -> 'Fold'       s a -> 'Expectation'
-- shouldNotHave :: 'Show' s => s -> 'Iso''       s a -> 'Expectation'
-- shouldNotHave :: 'Show' s => s -> 'Lens''      s a -> 'Expectation'
-- shouldNotHave :: 'Show' s => s -> 'Traversal'' s a -> 'Expectation'
-- shouldNotHave :: 'Show' s => s -> 'Prism''     s a -> 'Expectation'
-- @
shouldNotHave :: Show s => s -> Getting All s a -> Expectation
x `shouldNotHave` f = assertBool errorMsg (hasn't f x)
 where
  errorMsg = unwords ["Supplied Fold has targets for", show x]

-- | @x \`shouldView\` y \`through\` l@ sets the expectation that
-- you can see @y@ in @x@ though a 'Getter' @l@
--
-- @
-- shouldView ::           ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Getter'     s a -> 'Expectation'
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Fold'       s m -> 'Expectation'
-- shouldView ::           ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Iso''       s a -> 'Expectation'
-- shouldView ::           ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Lens''      s a -> 'Expectation'
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Traversal'' s m -> 'Expectation'
-- shouldView :: ('Data.Monoid.Monoid' m, 'Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Prism''     s m -> 'Expectation'
-- @
shouldView :: (Show s, Show a, Eq a) => s -> a -> Getting a s a -> Expectation
(x `shouldView` y) l = assertBool errorMsg (view l x == y)
 where
  errorMsg = unwords ["Can't view", show y, "from", show x, "through supplied Getter"]

-- | @x \`shouldPreview\` y \`through\` l@ sets the expectation that
-- you can list @y@ in @x@ first though a 'Fold' @l@
--
-- @
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Getter'     s a -> 'Expectation'
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Fold'       s a -> 'Expectation'
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Lens''      s a -> 'Expectation'
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Iso''       s a -> 'Expectation'
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Traversal'' s a -> 'Expectation'
-- shouldPreview :: ('Show' s, 'Show' a, 'Eq' a) => s -> a -> 'Prism''     s a -> 'Expectation'
-- @
shouldPreview :: (Show s, Show a, Eq a) => s -> a -> Getting (First a) s a -> Expectation
(x `shouldPreview` y) l = assertBool errorMsg (preview l x == Just y)
 where
  errorMsg = unwords ["Can't preview", show y, "from", show x, "through supplied Fold"]

-- | @x \`shouldList\` ys \`through\` l@ sets the expectation that
-- you can list @ys@ in @x@ though a 'Fold' @l@
--
-- @
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Getter'     s a -> 'Expectation'
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Fold'       s a -> 'Expectation'
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Lens''      s a -> 'Expectation'
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Iso''       s a -> 'Expectation'
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Traversal'' s a -> 'Expectation'
-- shouldList :: ('Show' s, 'Show' a, 'Eq' a) => s -> [a] -> 'Prism''     s a -> 'Expectation'
-- @
shouldList :: (Show s, Show a, Eq a) => s -> [a] -> Getting (Endo [a]) s a -> Expectation
(x `shouldList` y) l = assertBool errorMsg (toListOf l x == y)
 where
  errorMsg = unwords ["Can't list", show y, "from", show x, "through supplied Fold"]

-- | A helper to fight parentheses
--
-- @
-- through ≡ id
-- @
--
-- @
-- through :: 'Int' -> 'Int'
-- through :: 'Char' -> 'Char'
-- @
through :: a -> a
through = id
