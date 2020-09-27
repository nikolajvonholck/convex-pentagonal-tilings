module Permutation (dihedralGroup, symmetricGroup, permute) where

import Data.List (permutations, genericTake)
import Data.Map.Strict (Map, elems, fromList, (!?), empty, mapMaybe)

data Permutation = Permutation (Map Integer Integer)
  deriving (Show, Eq)

dihedralGroup :: Integer -> [Permutation]
dihedralGroup n =
  let rotations = genericTake n $ iterate rotate [1..n]
  in [Permutation (toMap p) | r <- rotations, p <- [r, reflect r]]
  where
    rotate :: [Integer] -> [Integer]
    rotate [] = []
    rotate (x:xs) = xs ++ [x]
    reflect :: [Integer] -> [Integer]
    reflect = reverse

symmetricGroup :: Integer -> [Permutation]
symmetricGroup n = [Permutation (toMap p) | p <- permutations [1..n]]

permute :: Permutation -> [a] -> [a]
permute (Permutation p) l = elems $ compose (toMap l) p

toMap :: [a] -> Map Integer a
toMap p = fromList $ enumerate p

-- 1-indexed
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1..]

compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab
