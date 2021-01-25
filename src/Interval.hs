module Interval (Interval, begin, end, fromElement, interval, leastContainingInterval, isElementOf, isElementOfInterior, extendWith, width, midpoint, offset) where

-- import Data.Ord (min, max)

data Interval a = I (a, a) deriving (Eq, Show)

begin :: Interval a -> a
begin (I (a, _)) = a

end :: Interval a -> a
end (I (_, b)) = b

fromElement :: a -> Interval a
fromElement x = I (x, x)

interval :: Ord a => (a, a) -> Interval a
interval (a, b)
  | a <= b = I (a, b)
  | otherwise = error "Degenerate interval."

leastContainingInterval :: Ord a => Interval a -> Interval a -> Interval a
leastContainingInterval (I (a1, b1)) (I (a2, b2)) = I (min a1 a2, max b1 b2)

isElementOf :: (Num a, Ord a) => a -> Interval a -> Bool
isElementOf x (I (a, b)) = a <= x && x <= b

isElementOfInterior :: (Num a, Ord a) => a -> Interval a -> Bool
isElementOfInterior x (I (a, b)) = a < x && x < b

extendWith :: Ord a => a -> Interval a -> Interval a
extendWith x i = leastContainingInterval i $ fromElement x

width :: Num a => Interval a -> a
width (I (a, b)) = b - a

midpoint :: Fractional a => Interval a -> a
midpoint (I (a, b)) = (a + b) / 2

offset :: (Num a, Ord a) => a -> Interval a -> Interval a
offset x (I (a, b)) = interval (a + x, b + x)
