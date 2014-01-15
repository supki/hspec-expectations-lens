module Test.Hspec.Lens
  ( -- * Types
    Spec
  , Example
    -- * Setting expectations
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
