module Polynomial (Polynomial, polynomial, add, mul, neg, sub, evaluate, euclideanDivision, bound, degreeWithLeadingCoefficient, zeroPolynomial, constant, degree, derivative) where

import Interval (Interval, interval, fromElement, begin, end, midpoint, width)
import Utils (maxBy)

import qualified Data.Map.Strict as Map
import Data.Map (Map, toList, empty, filterWithKey, unionWith)

-- The coeffcients should be non-zero.
data Polynomial a = Poly (Map Integer a) deriving (Eq, Show)

zeroPolynomial :: Polynomial a
zeroPolynomial = Poly empty

degreeWithLeadingCoefficient :: (Num a) => Polynomial a -> Maybe (Integer, a)
degreeWithLeadingCoefficient (Poly f) = case toList f of
  [] -> Nothing
  ics -> Just $ maxBy fst ics

degree :: (Num a) => Polynomial a -> Integer
degree f = case degreeWithLeadingCoefficient f of
    Nothing -> -1
    Just (i, _) -> i

-- Simplifies polynomial by removing zero-coeffcients.
polynomial :: (Num a, Eq a) => Map Integer a -> Polynomial a
polynomial m = Poly $ filterWithKey (\i c -> i >= 0 && c /= 0) m

fromList :: (Num a, Eq a) => [(Integer, a)] -> Polynomial a
fromList = polynomial . Map.fromList

constant :: (Num a, Eq a) => a -> Polynomial a
constant x = fromList [(0, x)]

derivative :: (Num a, Eq a) => Polynomial a -> Polynomial a
derivative (Poly f) =
  fromList [(i - 1, fromInteger i * c) | (i, c) <- toList f, i > 0]

evaluate :: (Fractional a, Eq a) => Polynomial a -> a -> a
evaluate (Poly f) a = sum [c * (a^^i) | (i, c) <- toList f]

-- Bounding interval for image of f over the given interval.
bound :: (Fractional a, Ord a) => Polynomial a -> Interval a -> Interval a
bound f i =
  case degreeWithLeadingCoefficient f of
    Nothing -> fromElement 0
    Just (0, c) -> fromElement c
    Just _ ->
      let (x, dx) = (midpoint i, width i / 2)
          slopeBound = bound (derivative f) i
          maxSlope = max (abs $ begin slopeBound) (abs $ end slopeBound)
          fx = evaluate f x
      in interval (fx - dx * maxSlope, fx + dx * maxSlope)

add :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
add (Poly f) (Poly g) = polynomial $ unionWith (+) f g

neg :: (Num a) => Polynomial a -> Polynomial a
neg (Poly f) = Poly (Map.map negate f)

sub :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
sub f g = add f (neg g)

mul :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
mul (Poly f) g =
  let terms = map (mulMonomial g) (toList f)
  in foldl add zeroPolynomial terms
  where
    mulMonomial :: (Num a, Eq a) => Polynomial a -> (Integer, a) -> Polynomial a
    mulMonomial (Poly h) (j, q) =
      fromList [(i + j, q * p) | (i, p) <- toList h]

-- Performs Euclidean division. The polynomial g should be non-zero.
euclideanDivision :: (Fractional a, Eq a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
euclideanDivision f g =
  case degreeWithLeadingCoefficient g of
    Nothing -> error "Euclidean division by zero polynomial."
    Just (j, a) -> case degreeWithLeadingCoefficient f of
      Nothing -> (zeroPolynomial, f)
      Just (i, b) ->
        if i < j then (zeroPolynomial, f)
          else
            let s = fromList [(i - j, b / a)]
                f' = f `sub` (s `mul` g)
                (q, r) = euclideanDivision f' g
            in (q `add` s, r)
