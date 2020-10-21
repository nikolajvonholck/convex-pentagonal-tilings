module Interval (Interval, fromElement, toInterval, leastContainingInterval, isElementOf, extendWith, width, midpoint, add, neg, sub, scale) where

-- import Data.Ord (min, max)

data Interval a = I (a, a) deriving (Eq)

fromElement :: a -> Interval a
fromElement x = I (x, x)

toInterval :: Ord a => a -> a -> Interval a
toInterval a b
  | a <= b = I (a, b)
  | otherwise = error "Degenerate interval."

leastContainingInterval :: Ord a => Interval a -> Interval a -> Interval a
leastContainingInterval (I (a1, b1)) (I (a2, b2)) = I (min a1 a2, max b1 b2)

isElementOf :: (Num a, Ord a) => Interval a -> a -> Bool
isElementOf (I (a, b)) x = a <= x && x <= b

extendWith :: Ord a => Interval a -> a -> Interval a
extendWith i x = leastContainingInterval i $ fromElement x

width :: Num a => Interval a -> a
width (I (a, b)) = b - a

midpoint :: Fractional a => Interval a -> a
midpoint (I (a, b)) = (a + b) / 2

add :: Num a => Interval a -> Interval a -> Interval a
add (I (a1, b1)) (I (a2, b2)) = I (a1 + a2, b1 + b2)

neg :: Num a => Interval a -> Interval a
neg (I (a, b)) = I (-b, -a)

sub :: Num a => Interval a -> Interval a -> Interval a
sub i1 i2 = add i1 (neg i2)

scale :: (Num a, Ord a) => Interval a -> a -> Interval a
scale (I (a, b)) m
  | m >= 0 = I (m * a, m * b)
  | otherwise  = I (m * b, m * a)
