{-# options_ghc -Wno-orphans #-}

module Orphans where

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe
import Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    NonEmpty xs <- arbitrary
    return $ case xs of
      [ ] -> error "QuickCheck promises to give a non-empty list."
      (x: xs') -> x :| xs'
  shrink = mapMaybe NonEmpty.nonEmpty . shrink . NonEmpty.toList
