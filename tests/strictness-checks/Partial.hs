{-# language
    TypeApplications, FlexibleInstances, TypeFamilies, DeriveFunctor
    , DeriveTraversable, GeneralizedNewtypeDeriving, PatternSynonyms
    , FlexibleContexts, ConstraintKinds
  #-}

module Partial where

import Control.DeepSeq
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Bitraversable
import qualified Data.List as List
import Numeric.Natural
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, frequency)

import Prelude'

-- | A specification for a value that might not be defined.
newtype Specification value = Specification {partial :: Maybe value} deriving (Show, Functor, Foldable, Traversable)
pattern Undefined :: Specification value
pattern Undefined = Specification Nothing
pattern Defined :: value -> Specification value
pattern Defined value = Specification (Just value)

instance {-# overlappable #-} Arbitrary value => Arbitrary (Specification value) where
  arbitrary = generatePartial arbitrary
  shrink (Specification (Just value)) = Undefined: fmap Defined (shrink value)
  shrink (Specification Nothing) = [ ]

generatePartial :: Gen value -> Gen (Specification value)
generatePartial generatorOfValues = frequency
      [ (1, return (Specification Nothing))
      , (9, fmap (Specification . Just) generatorOfValues) ]

-- | Extract an error from a value that might be somewhere undefined.
save :: NFData value => value -> IO (Either String value)
save = fmap (first errorMessage) . try @ErrorCall . evaluate . force

-- | Total values are returned as they are. They are never looked into.
newtype Total value = Total {total :: value} deriving (Functor, Foldable, Traversable, Arbitrary)
instance Show value => Show (Total value)
  where
    show = show . total

type family MapPartial (typeConstructor :: * -> *) value where
  MapPartial _ (Total value) = Total value
  MapPartial typeConstructor (Specification value) = typeConstructor (MapPartial typeConstructor value)
  MapPartial typeConstructor (trifunctor first second third) = trifunctor (MapPartial typeConstructor first) (MapPartial typeConstructor second) (MapPartial typeConstructor third)
  MapPartial typeConstructor (bifunctor first second) = bifunctor (MapPartial typeConstructor first) (MapPartial typeConstructor second)
  MapPartial typeConstructor (functor value) = functor (MapPartial typeConstructor value)
  MapPartial _ anythingElse = anythingElse

newtype Labelled value = Labelled {labelled :: Either [Natural] value} deriving (Functor, Foldable, Traversable)
instance Show value => Show (Labelled value) where
  show (Labelled (Right value)) = show value
  show (Labelled (Left ordinals)) = "\"" ++ "#" ++ (List.intercalate "/" . reverse . fmap show) ordinals ++ "\""

-- | Provide a discernible label for every value that is not defined.
label :: Specification value -> Reader [Natural] (Labelled value)
label = maybe (fmap (Labelled . Left) ask) (return . Labelled . Right) . partial

class LabelRecursively partial
  where
    labelRecursively :: partial -> Reader [Natural ] (MapPartial Labelled partial)

instance LabelRecursively (Total value)
  where
    labelRecursively = return

instance LabelRecursively value => LabelRecursively (Specification value)
  where
    labelRecursively = (bind . traverse) labelRecursively . label

instance (LabelRecursively a, LabelRecursively b, LabelRecursively c) => LabelRecursively (a, b, c)
  where
    labelRecursively (a, b, c) = pure (, , ) <*> local (0: ) (labelRecursively a) <*> local (1: ) (labelRecursively b) <*> local (2: ) (labelRecursively c)

instance {-# overlappable #-}
  ( Bitraversable bifunctor
  , LabelRecursively firstValue
  , LabelRecursively secondValue
  , MapPartial Labelled (bifunctor firstValue secondValue) ~ bifunctor (MapPartial Labelled firstValue) (MapPartial Labelled secondValue)
  ) => LabelRecursively (bifunctor firstValue secondValue)
  where
    labelRecursively = flip evalStateT (0 :: Natural) . bitraverse labelRecursivelyAndIncreaseIndex labelRecursivelyAndIncreaseIndex

instance {-# overlappable #-}
  ( Traversable functor
  , LabelRecursively value
  , MapPartial Labelled (functor value) ~ functor (MapPartial Labelled value)
  ) => LabelRecursively (functor value)
  where
    labelRecursively = flip evalStateT (0 :: Natural) . traverse labelRecursivelyAndIncreaseIndex

instance {-# overlappable #-} value ~ MapPartial Labelled value => LabelRecursively value
  where
    labelRecursively = return

labelRecursivelyAndIncreaseIndex
  :: LabelRecursively value
  => value -> StateT Natural (Reader [Natural]) (MapPartial Labelled value)
labelRecursivelyAndIncreaseIndex value = do
  currentIndex <- get
  modify (+ 1)
  lift (local (currentIndex: ) (labelRecursively value))

evaluateLabels :: Reader [Natural] value -> value
evaluateLabels = flip runReader [ ]

observe ::
  ( LabelRecursively value
  ) => value -> MapPartial Labelled value
observe = evaluateLabels . labelRecursively

type family Unlabelled labelledValue where
  Unlabelled (Total value) = value
  Unlabelled (Labelled value) = Unlabelled value
  Unlabelled (trifunctor first second third) = trifunctor (Unlabelled first) (Unlabelled second) (Unlabelled third)
  Unlabelled (bifunctor first second) = bifunctor (Unlabelled first) (Unlabelled second)
  Unlabelled (functor value) = functor (Unlabelled value)
  Unlabelled anythingElse = anythingElse

class Undefine value
  where
    undefine :: value -> Unlabelled value

instance Undefine (Total value)
  where
    undefine = total

instance (Undefine value, Show value) => Undefine (Labelled value)
  where
    undefine locationOfError@(Labelled (Left _)) = (error . show) locationOfError
    undefine (Labelled (Right value)) = undefine value

instance (Undefine a, Undefine b, Undefine c) => Undefine (a, b, c)
  where
    undefine (a, b, c) = (undefine a, undefine b, undefine c)

instance {-# overlappable #-}
  ( Bifunctor bifunctor
  , Undefine firstValue
  , Undefine secondValue
  , Unlabelled (bifunctor firstValue secondValue) ~ bifunctor (Unlabelled firstValue) (Unlabelled secondValue)
  ) => Undefine (bifunctor firstValue secondValue)
  where
    undefine = bimap undefine undefine

instance {-# overlappable #-}
  ( Functor functor
  , Undefine value
  , Unlabelled (functor value) ~ functor (Unlabelled value)
  ) => Undefine (functor value)
  where
    undefine = fmap undefine

instance {-# overlappable #-} value ~ Unlabelled value => Undefine value
  where
    undefine = id

-- | A type for specifications turned into appropriately undefined values.
type Instantiated value = Unlabelled (MapPartial Labelled value)

type Specifiable value = Undefine (MapPartial Labelled value)

-- | Given a function and a specification of its input, see if and where it errors out.
withErrors ::
  ( LabelRecursively specification
  , Specifiable specification
  , NFData output
  ) => (Instantiated specification -> output) -> specification -> IO (Either String output)
withErrors function = save . function . undefine . evaluateLabels . labelRecursively
