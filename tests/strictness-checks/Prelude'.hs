{-# language DeriveTraversable, GeneralizedNewtypeDeriving, TypeApplications, ScopedTypeVariables #-}

module Prelude' where

import Control.Exception
import Data.Coerce
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
