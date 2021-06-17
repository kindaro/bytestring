{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts, TypeApplications #-}

module Main where

import Control.DeepSeq
import Data.Coerce
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

foldrs :: [(String, V2 ((Named (Word8 -> Int -> Int), Int, ([ArbitraryNonEmpty Word8], [ArbitraryNonEmpty Word8])) -> Int))]
foldrs =
  [ ( "foldr" , V2
      ( \ (Named _name function, initialAccumulator, (headingChunks, trailingChunks)) ->
          let byteStream = chunksToByteStream (headingChunks ++ trailingChunks)
          in Lazy.foldr function initialAccumulator byteStream
      , \ (Named _name function, initialAccumulator, (headingChunks, trailingChunks)) ->
          let bytes = fmap NonEmpty.toList (coerce headingChunks ++ coerce trailingChunks :: [NonEmpty Word8])
          in Foldable.foldr (flip (Foldable.foldr' function)) initialAccumulator bytes ) )
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
  , Specification ([Specification (ArbitraryNonEmpty Word8)], Specification [Specification (ArbitraryNonEmpty Word8)]) )
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

chunksToList :: [ArbitraryNonEmpty Word8] -> [Word8]
chunksToList = coerce actualFunction
  where
    actualFunction :: [NonEmpty Word8] -> [Word8]
    actualFunction = concatMap NonEmpty.toList

chunksToByteStream :: [ArbitraryNonEmpty Word8] -> Lazy.ByteString
chunksToByteStream = coerce actualFunction
  where
    actualFunction :: [NonEmpty Word8] -> Lazy.ByteString
    actualFunction = foldr (Lazy.Chunk . Strict.pack . NonEmpty.toList) Lazy.Empty
