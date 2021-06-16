{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Main where

import Control.DeepSeq
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Word
import Data.Bitraversable

import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy

import Test.Tasty
import Test.Tasty.QuickCheck (Gen, Property, (===), Arbitrary (arbitrary), testProperty)
import qualified Test.Tasty.QuickCheck as QuickCheck

import Orphans ( )
import Partial
import Prelude'

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testGroup "…"
    [ testGroup nameOfFunction
      [ makeTestTree namedFunction namedGenerators
      | namedGenerators <- (fmap . fmap) elaborateGenerator parameterFunctions ]
    | namedFunction@(nameOfFunction, _) <- foldrs
    ]
  ]

foldrs :: [(String, V2 ((Named (Word8 -> Int -> Int), Int, ([NonEmpty Word8], [NonEmpty Word8])) -> Int))]
foldrs =
  [ ( "foldr" , V2
      ( \ (function, initalAccumulator, listOfChunks) -> Lazy.foldr (named function) initalAccumulator ((chunksToByteStream . uncurry (++)) listOfChunks)
      , \ (function, initalAccumulator, listOfChunks) -> Foldable.foldr (named function) initalAccumulator ((chunksToList . uncurry (++)) listOfChunks) ) )
  ]

parameterFunctions :: [(String, Gen (Named (Word8 -> Int -> Int)))]
parameterFunctions =
  [ ("lazy", sequence Named {name = "(fmap (const . const) arbitrary)", named = fmap (const . const) arbitrary})
  , ("strict on the left", sequence Named {name = "(return (const . fromIntegral))", named = return (const . fromIntegral)})
  , ("strict on the right", sequence Named {name = "(return (flip const))", named = return (flip const)})
  , ("strict", sequence Named {name = "(return ((+) . fromIntegral))", named = return ((+) . fromIntegral)})
  ]

elaborateGenerator :: Gen (Named (Word8 -> Int -> Int)) -> Gen
  ( Specification (Total (Named (Word8 -> Int -> Int)))
  , Specification Int
  , Specification ([Specification (NonEmpty (Specification Word8))], Specification [Specification (NonEmpty (Specification Word8))]) )
elaborateGenerator generator = do
  function <- generatePartial generator
  int <- arbitrary
  chunks <- arbitrary
  return (fmap Total function, int, chunks)

makeTestTree ::
  ( LabelRecursively specification, Specifiable specification, Show (MapPartial Labelled specification), Arbitrary specification
  , NFData output, Eq output, Show output
  , Instantiated specification ~ input
  ) => (String, V2 (input -> output)) -> (String, Gen specification) -> TestTree
makeTestTree (nameOfFunction, functionAndOracle) (nameOfGenerator, generator)
  = testProperty (nameOfFunction ++ " — " ++ nameOfGenerator) (extensionalEqualityWithStrictness functionAndOracle generator)

extensionalEqualityWithStrictness :: forall specification input output.
  ( LabelRecursively specification, Specifiable specification, Show (MapPartial Labelled specification), Arbitrary specification
  , NFData output, Eq output, Show output
  , Instantiated specification ~ input
  ) => V2 (input -> output) -> Gen specification -> Property
extensionalEqualityWithStrictness (V2 (function, oracle)) generatorOfPartialInput
  = QuickCheck.forAllShrinkShow generatorOfPartialInput QuickCheck.shrink (show . Pretty . observe)
  $ \ specification -> QuickCheck.ioProperty $ do
  (expected, actual) <- bisequence (fork ($ oracle) ($ function) (($ specification) . withErrors))
  let comparison = Pretty actual === Pretty expected
  return $ case expected of
    Left labelOfError -> QuickCheck.label labelOfError comparison
    Right _ -> QuickCheck.label "defined" comparison

chunksToList :: [NonEmpty Word8] -> [Word8]
chunksToList = concatMap NonEmpty.toList

chunksToByteStream :: [NonEmpty Word8] -> Lazy.ByteString
chunksToByteStream = foldr (Lazy.Chunk . Strict.pack . NonEmpty.toList) Lazy.Empty
