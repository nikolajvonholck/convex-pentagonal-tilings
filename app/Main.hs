module Main where

import GoodSubsets
import Permutation
import Data.Set (empty, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

s5 = symmetricGroup 5
d5n = dihedralGroup 5

main :: IO ()
main = do
  putStrLn "Will determine all non-empty maximal good sets..."
  let maximalSets = recurse empty Map.empty
  let maximalGoodSets = Map.filter id maximalSets
  let nonEmptyMaximalGoodSets = Map.keysSet maximalGoodSets \\ singleton empty -- non-empty
  putStrLn . show $ size nonEmptyMaximalGoodSets -- 193 √
  let allPermutations = unions $ Set.map (\xs -> fromList [Set.map (permute p) xs | p <- s5]) nonEmptyMaximalGoodSets
  putStrLn . show $ size allPermutations -- 3495 √
  let reps = unique (toList allPermutations) empty
  putStrLn . show $ length reps -- 371 √
  where
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList [Set.map (permute p) vs | p <- d5n])
