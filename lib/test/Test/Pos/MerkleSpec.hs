-- | "Pos.Merkle" specification

module Test.Pos.MerkleSpec
       ( spec
       ) where


import           Universum

import qualified Data.Foldable as Foldable (length, toList)
import           Test.Hspec (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, (===))

import           Pos.Binary (Bi)
import           Pos.Core.Merkle (mkMerkleTree)
import           Test.Pos.Binary.Helpers (safeCopyEncodeDecode)

spec :: Spec
spec = describe "Merkle" $ do
    prop
        "toList . mkMerkleTree === id"
        (generateAndFoldProp @Int32)
    prop
        "size . mkMerkleTree === length"
        (sizeProp @Int32)
    prop
        "safeCopy instance, get === put^-1"
        (safeProp @Int32)

generateAndFoldProp :: (Eq a, Show a, Bi a) => [a] -> Property
generateAndFoldProp xs = Foldable.toList (mkMerkleTree xs) === xs

sizeProp :: (Bi a) => [a] -> Property
sizeProp xs = Foldable.length (mkMerkleTree xs) === fromIntegral (Foldable.length xs)

safeProp :: (Eq a, Show a, Bi a) => [a] -> Property
safeProp xs = let m = mkMerkleTree xs in safeCopyEncodeDecode m
