module Prelude' where

import Control.Exception
import Test.QuickCheck
import Text.Show.Pretty

errorMessage :: ErrorCall -> String
errorMessage (ErrorCall string) = string

bind :: Monad monad => (input -> monad output) -> monad input -> monad output
bind = (=<<)

newtype V2 a = V2 (a, a)

data Named value = Named {name :: String, named :: value} deriving (Functor, Foldable, Traversable)
instance Show (Named value) where show = name
instance Arbitrary value => Arbitrary (Named value)
  where
    arbitrary = sequence Named {name = "", named = arbitrary}
    shrink = traverse shrink

newtype Pretty value = Pretty {pretty :: value} deriving (Eq, Arbitrary)
instance Show value => Show (Pretty value) where show = ppShow . pretty
