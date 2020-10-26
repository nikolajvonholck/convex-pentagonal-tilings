module Polynomial (Polynomial, polynomial, add, mul, neg, sub, evaluate, euclideanDivision, bound, degreeWithLeadingCoefficient, zeroPolynomial, constant) where

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

-- degree :: (Num a) => Polynomial a -> Integer
-- degree f = case degreeWithLeadingCoefficient f of
--     Nothing -> -1
--     Just (i, _) -> i

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

-- TODO: slet // Bounding interval of f(a + x) for |x| <= l. Assumes l >= 0.
-- TODO: Check math.
-- Bounding interval of f(z) for z in interval.
bound :: (Fractional a, Ord a) => Polynomial a -> Interval a -> Interval a
bound f i =
  case degreeWithLeadingCoefficient f of
    Nothing -> fromElement 0
    Just (0, c) -> fromElement c
    Just _ ->
      let (x, w) = (midpoint i, width i / 2)
          bound' = bound (derivative f) i
          d = max (abs $ begin bound') (abs $ end bound')
          fx = evaluate f x
      in interval (fx - w * d, fx + w * d)

add :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
add (Poly f) (Poly g) = polynomial $ unionWith (+) f g

neg :: (Num a) => Polynomial a -> Polynomial a
neg (Poly f) = Poly (Map.map negate f)

sub :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
sub f g = add f (neg g)

mulMonomial :: (Num a, Eq a) => Polynomial a -> (Integer, a) -> Polynomial a
mulMonomial (Poly f) (j, q) =
  fromList [(i + j, q * p) | (i, p) <- toList f]

mul :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
mul (Poly f) g =
  let terms = map (mulMonomial g) (toList f)
  in foldl add zeroPolynomial terms

-- TODO: State assumptions.
-- TODO: Check math.
euclideanDivision :: (Fractional a, Eq a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
euclideanDivision f g =
  case degreeWithLeadingCoefficient g of
    Nothing -> error "Euclidean division by zero polynomial."
    Just (j, a) -> case degreeWithLeadingCoefficient f of
      Nothing -> (zeroPolynomial, f)
      Just (i, b) ->
        if i < j then (zeroPolynomial, f)
          else
            let monomial = fromList [(i - j, b / a)]
                f' = f `sub` (monomial `mul` g)
                (q, r) = euclideanDivision f' g
            in (q `add` monomial, r)
