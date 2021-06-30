{-# language DeriveTraversable, GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables #-}

module Prelude' where

import Control.Exception
import Data.Coerce
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty ( ))
import Data.Maybe
import Test.QuickCheck
import Text.Groom

errorMessage :: ErrorCall -> String
errorMessage (ErrorCall string) = string

bind :: Monad monad => (input -> monad output) -> monad input -> monad output
bind = (=<<)

fork :: (input -> leftOutput) -> (input -> rightOutput) -> input -> (leftOutput, rightOutput)
fork leftFunction rightFunction input = (leftFunction input, rightFunction input)

newtype V2 a = V2 (a, a)

data Named value = Named {name :: String, named :: value} deriving (Functor, Foldable, Traversable)
instance Show (Named value) where show = name
instance Arbitrary value => Arbitrary (Named value)
  where
    arbitrary = sequence Named {name = "", named = arbitrary}
    shrink = traverse shrink

newtype Pretty value = Pretty {pretty :: value} deriving (Eq, Arbitrary)
instance Show value => Show (Pretty value) where show = groom . pretty

newtype ArbitraryNonEmpty item = ArbitraryNonEmpty (NonEmpty item)
  deriving (Functor, Foldable, Show, Traversable)
instance Arbitrary item => Arbitrary (ArbitraryNonEmpty item) where
  arbitrary = coerce (arbitraryNonEmpty @item)
  shrink = coerce (shrinkNonEmpty @item)

shrinkNonEmpty :: Arbitrary item => NonEmpty item -> [NonEmpty item]
shrinkNonEmpty = mapMaybe NonEmpty.nonEmpty . shrink . NonEmpty.toList

arbitraryNonEmpty :: Arbitrary item => Gen (NonEmpty item)
arbitraryNonEmpty = do
  NonEmpty xs <- arbitrary
  return $ case xs of
    [ ] -> error "QuickCheck promises to give a non-empty list."
    (x: xs') -> x NonEmpty.:| xs'

data StrictList a = !a :! StrictList a | StrictEmpty deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

listToStrictList :: [a] -> StrictList a
listToStrictList = foldr (:!) StrictEmpty

instance Arbitrary a => Arbitrary (StrictList a) where
  arbitrary = fmap listToStrictList arbitrary
  shrink = fmap listToStrictList . shrink . Foldable.toList

instance Semigroup (StrictList a) where
  xs <> ys = listToStrictList (Foldable.toList xs <> Foldable.toList ys)

instance Monoid (StrictList a) where
  mempty = StrictEmpty

shouldBe :: (Eq value, Show value) => value -> value -> Property
actual `shouldBe` expected
  = counterexample ("Actual:  \t" ++ show (Pretty actual))
  . counterexample ("Expected:\t" ++ show (Pretty expected))
  $ actual == expected
