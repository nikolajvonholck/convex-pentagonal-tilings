module Main where

import GoodSubsets (VectorTypeSet, recurse, inflate, permutations, rotationsAndReflections)
import Data.Set (Set, empty, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "Will determine all non-empty maximal good sets..."
  let maximalSets = recurse
  let maximalGoodSets = Map.filter id maximalSets
  let nonEmptyMaximalGoodSets = Map.keysSet maximalGoodSets \\ singleton empty
  print $ size nonEmptyMaximalGoodSets -- 193 √
  let allPermutations = unions $ Set.map (fromList . permutations) nonEmptyMaximalGoodSets
  print $ size allPermutations -- 3495 √
  let representatives = unique (toList allPermutations) empty
  print $ length representatives -- 371 √
  putStrLn "Will compare with Michael Rao's results..."
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map (inflate . fromList) raoGenerators of
        Nothing -> error "Failed to compute Michael Rao's maximal good subsets."
        (Just goodSubsets) -> goodSubsets
  putStrLn "Have we found the same maximal good subsets?"
  print $ (raoGoodSubsets `isSubsetOf` representatives) && (representatives `isSubsetOf` raoGoodSubsets)
  where
    isSubsetOf :: [VectorTypeSet] -> [VectorTypeSet] -> Bool
    a `isSubsetOf` b = all (\vs -> any (`elem` b) $ rotationsAndReflections vs) a
    unique :: [VectorTypeSet] -> Set VectorTypeSet -> [VectorTypeSet]
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList $ rotationsAndReflections vs)
