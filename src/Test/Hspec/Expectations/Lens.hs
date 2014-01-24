-- | Hspec expectations for the lens stuff
module Test.Hspec.Expectations.Lens
  ( -- * Expectations
    shouldHave
  , shouldNotHave
  , shouldView
  , shouldPreview
  , shouldList
  , shouldThrow
  , shouldPerform
  , through
  ) where

import Control.Lens
import Control.Monad
import Control.Exception (SomeException)
import Control.Exception.Lens
import Data.Monoid (Any(..), All(..), First(..), Endo(..))
import Test.Hspec.Expectations (Expectation)
import Test.HUnit (assertBool, assertFailure)
import Text.Printf (printf)


infixl 1 `shouldHave`, `shouldNotHave`
infixl 1 `shouldView`, `shouldPreview`, `shouldList`
infixl 1 `shouldThrow`
infixl 1 `shouldPerform`
infixl 1 `through`

-- | @s \`shouldHave\` l@ sets the expectation that 'Fold' @l@ has
-- non-zero number of targets in the structure @s@
--
-- > s `shouldBe` t ≡ s `shouldHave` only t
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
s `shouldHave` l = assertBool msg (has l s)
 where
  msg = printf "This Fold has zero targets for %s" (show s)

-- | @s \`shouldNotHave\` l@ sets the expectation that 'Fold' @l@
-- has exactly zero targets in the structue @s@
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
s `shouldNotHave` l = assertBool msg (hasn't l s)
 where
  msg = printf "This Fold has non-zero targets for %s" (show s)

-- | @s \`shouldView\` t \`through\` l@ sets the expectation that
-- you can see target @t@ in the structure @s@ though a 'Getter' @l@
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
(s `shouldView` t) l =
  let r = view l s in
    unless (r == t) $
      assertFailure (printf "Resulted in %s, but expected %s" (show r) (show t))

-- | @s \`shouldPreview\` t \`through\` l@ sets the expectation that
-- you @y@ is the first target of the 'Fold' @l@ in @s@
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
(s `shouldPreview` t) l =
  case preview l s of
    Nothing ->
      assertFailure (printf "No targets, but expected %s" (show t))
    Just r  -> unless (r == t) $
      assertFailure (printf "Resulted in %s, but expected %s" (show r) (show t))

-- | @s \`shouldList\` ts \`through\` l@ sets the expectation that
-- @ts@ is a list of the Fold @l@ targets in @x@
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
(s `shouldList` t) l =
  let r = toListOf l s in
    unless (r == t) $
      assertFailure (printf "Resulted in %s, but expected %s" (show r) (show t))

-- | @a \`shouldThrow\` l@ sets the expectation that
-- @a@ throws an exception that 'Fold' @l@ can catch
--
-- "Test.Hspec" exports 'Test.Hspec.Expectations.shouldThrow' too; it
-- only allows @e -> Bool@ selectors, which is less general and often less convenient
--
-- @
-- shouldThrow :: 'IO' a -> 'Getter'     s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> 'Fold'       s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> 'Lens''      s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> 'Iso''       s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> 'Traversal'' s b -> 'Expectation'
-- shouldThrow :: 'IO' a -> 'Prism''     s b -> 'Expectation'
-- @
shouldThrow :: IO a -> Getting (First b) SomeException b -> Expectation
x `shouldThrow` l = do
  r <- trying l x
  case r of
    Left  _ -> return ()
    Right _ -> assertFailure "Couldn't catch any exceptions"

-- | @a \`shouldPerform\` t \`through\` l@ sets the expectation that @t@ is
-- a target of the 'MonadicFold' @l@ applied to the result of action @a@
--
-- @
-- shouldPerform :: ('Show' a, 'Eq' a) => 'IO' s -> a -> 'Action'      'IO' s a -> 'Expectation'
-- shouldPerform :: ('Show' a, 'Eq' a) => 'IO' s -> a -> 'MonadicFold' 'IO' s a -> 'Expectation'
-- @
shouldPerform :: (Show a, Eq a) => IO s -> a -> Acting IO (Leftmost a) s s a b -> Expectation
(x `shouldPerform` t) l = do
  r <- x ^!? acts.l
  case r of
    Nothing ->
      assertFailure (printf "No targets, but expected %s" (show t))
    Just r' -> unless (r' == t) $
      assertFailure (printf "Resulted in %s, but expected %s" (show r') (show t))

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
