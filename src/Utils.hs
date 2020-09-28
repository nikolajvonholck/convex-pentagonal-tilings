module Utils ((!), enumerate, findIndex, elemIndex, replaceAt, minBy, delta) where

import qualified Data.List as List
import Data.Foldable (minimumBy)
import Data.Ord (comparing)

-- | 1-indexed and generic version of '(!!)'.
(!) :: [a] -> Integer -> a
xs ! n = xs `List.genericIndex` (n - 1)

-- | 1-indexed enumeration.
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1..]

-- | 1-indexed and generic version of findIndex.
findIndex :: (a -> Bool) -> [a] -> Maybe Integer
findIndex f xs = fmap ((+1) . toInteger) $ List.findIndex f xs

-- | 1-indexed and generic version of elemIndex.
elemIndex :: Eq a => a -> [a] -> Maybe Integer
elemIndex x xs = fmap ((+1) . toInteger) $ List.elemIndex x xs

-- | 1-indexed replacement.
replaceAt :: Integer -> a -> [a] -> [a]
replaceAt i x xs = List.genericTake (i - 1) xs ++ x : (List.genericDrop i xs)

-- | Version of 'minimumBy' using default ordering.
minBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
minBy = minimumBy . comparing

-- | Return 1 if 'y' equals 'x'; 0 otherwise.
delta :: (Eq b, Num a) => b -> b -> a
delta x y = if x == y then 1 else 0
