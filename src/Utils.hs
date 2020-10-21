module Utils ((!), enumerate, findIndex, elemIndex, replaceAt, minBy, maxBy, extract, delta, zipPedantic, zipWithPedantic) where

import qualified Data.List as List
import Data.Foldable (minimumBy, maximumBy)
import Data.Ord (comparing)

import Control.Monad (guard)

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

-- | Version of 'maximumBy' using default ordering.
maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

extract :: (a -> Bool) -> [a] -> Maybe ([a], (Integer, a), [a])
extract p = (extract' p) . enumerate
  where
    extract' :: (a -> Bool) -> [(Integer, a)] -> Maybe ([a], (Integer, a), [a])
    extract' _ [] = Nothing
    extract' p' ((i, x):xs) = case extract' p' xs of
      (Just (xs', ix, xs'')) -> Just (x:xs', ix, xs'')
      Nothing -> do
          guard $ p' x
          return ([], (i, x), map snd xs)

-- | Return 1 if 'y' equals 'x'; 0 otherwise.
delta :: (Eq b, Num a) => b -> b -> a
delta x y = if x == y then 1 else 0

-- | Version of zip requiring lists to be of the same length.
zipPedantic :: [a] -> [b] -> [(a, b)]
zipPedantic [] [] = []
zipPedantic (x:xs) (y:ys) = (x, y):zipPedantic xs ys
zipPedantic _ _ = error "Will not zip lists of different lengths."

-- | Variation of zipWith requiring lists to be of the same length.
zipWithPedantic :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPedantic f xs ys = map (uncurry f) $ zipPedantic xs ys
