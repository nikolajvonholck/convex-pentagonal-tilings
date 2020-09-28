module Main where

import GoodSubsets (recurse)
import Permutation (permute, symmetricGroup, dihedralGroup)
import Data.Set (empty, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "Will determine all non-empty maximal good sets..."
  let maximalSets = recurse empty Map.empty
  let maximalGoodSets = Map.filter id maximalSets
  let nonEmptyMaximalGoodSets = Map.keysSet maximalGoodSets \\ singleton empty
  print $ size nonEmptyMaximalGoodSets -- 193 √
  let allPermutations = unions $ Set.map (\xs -> fromList [Set.map (permute p) xs | p <- s5]) nonEmptyMaximalGoodSets
  print $ size allPermutations -- 3495 √
  let representatives = unique (toList allPermutations) empty
  print $ length representatives -- 371 √
  where
    s5 = symmetricGroup 5
    d5n = dihedralGroup 5
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList [Set.map (permute p) vs | p <- d5n])
