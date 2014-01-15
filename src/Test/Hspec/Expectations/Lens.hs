-- | Hspec expectations for the lens stuff
module Test.Hspec.Expectations.Lens
  ( -- * Expectations
    shouldHave
  , shouldNotHave
  , shouldView
  , shouldPreview
  , shouldList
  , shouldThrow
  , through
  ) where

import Control.Lens
import Control.Exception (SomeException)
import Control.Exception.Lens
import Data.Monoid (Any(..), All(..), First(..), Endo(..))
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (assertBool, assertFailure)
import Text.Printf (printf)


infixl 1 `shouldHave`, `shouldNotHave`, `shouldView`, `shouldPreview`, `shouldList`, `shouldThrow`, `through`

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
x `shouldHave` f = assertBool msg (has f x)
 where
  msg = printf "Supplied Fold has zero targets for %s" (show x)

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
x `shouldNotHave` f = assertBool msg (hasn't f x)
 where
  msg = printf "Supplied Fold has non-zero targets for %s" (show x)

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
(x `shouldView` y) l = assertBool msg (view l x == y)
 where
  msg = printf "Can't view %s from %s through supplied Getter" (show y) (show x)

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
(x `shouldPreview` y) l = assertBool msg (preview l x == Just y)
 where
  msg = printf "Can't preview %s from %s through supplied Fold" (show y) (show x)

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
(x `shouldList` y) l = assertBool msg (toListOf l x == y)
 where
  msg = printf "Can't list %s from %s through supplied Fold" (show y) (show x)

-- | @x \`shouldthrow\` l@ sets the expectation that
-- @x@ throws an exception catchable with a 'Fold' @l@
--
-- /Note:/ name conflicts with 'Test.Hspec.Expectations.shouldThrow'
--
-- @
-- shouldThrow :: 'IO' a -> -> 'Getter'     s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> -> 'Fold'       s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> -> 'Lens''      s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> -> 'Iso''       s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> -> 'Traversal'' s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> -> 'Prism''     s b -> 'Expectation'
-- @
shouldThrow :: IO a -> Getting (First b) SomeException b -> Expectation
x `shouldThrow` l = do
  r <- trying l x
  case r of
    Left  _ -> return ()
    Right _ -> assertFailure "Couldn't catch any exceptions with the supplied Fold"

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
