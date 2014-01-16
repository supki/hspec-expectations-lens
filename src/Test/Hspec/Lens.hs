-- | Module is designed to be conveniently imported instead of "Test.Hspec"
--
-- It reexports "Test.Hspec" without expectations (except for `shouldBe`)
-- and "Test.Hspec.Expectations.Lens" expectations
module Test.Hspec.Lens
  ( -- * Types
    Spec
  , Example
    -- * Setting expectations
  , shouldBe
  , module Test.Hspec.Expectations.Lens
    -- * Defining a spec
  , describe
  , context
  , it
  , example
  , pending
  , pendingWith
  , before
  , after
  , around
  , parallel
    -- * Running a spec
  , hspec
  ) where

import Test.Hspec
import Test.Hspec.Expectations.Lens
