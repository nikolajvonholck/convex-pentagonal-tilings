module Polynomial (Polynomial, polynomial, evaluate, euclideanDivision, bound, degreeWithLeadingCoefficient, constant, degree, derivative, fromList, signedRemainderSequence, extendedSignedRemainderSequence) where

import Interval (Interval, interval, fromElement, begin, end, midpoint, width)
import Utils (maxBy)

import qualified Data.Map.Strict as Map
import Data.Map (Map, toList, filterWithKey, unionWith)
import Control.Monad (guard)

-- The coeffcients should be non-zero.
data Polynomial a = Poly (Map Integer a) deriving (Eq, Show)


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

instance (Num a, Eq a) => Num (Polynomial a) where
  (Poly f) + (Poly g) = polynomial $ unionWith (+) f g

  (Poly f) * (Poly g) =
    let terms = map (mulMonomial $ toList g) (toList f)
    in foldl (+) 0 terms
    where
      mulMonomial :: (Num a, Eq a) => [(Integer, a)] -> (Integer, a) -> Polynomial a
      mulMonomial terms (j, q) =
        fromList [(i + j, q * p) | (i, p) <- terms]

  negate (Poly f) = Poly (Map.map negate f)

  fromInteger = constant . fromInteger

  abs _ = error "abs not implemented for Polynomial."
  signum _ = error "signum not implemented for Polynomial."

-- Performs Euclidean division. The polynomial g(x) should be non-zero.
euclideanDivision :: (Fractional a, Eq a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
euclideanDivision f g =
  case degreeWithLeadingCoefficient g of
    Nothing -> error "Euclidean division by zero polynomial."
    Just (j, b) -> case degreeWithLeadingCoefficient f of
      Nothing -> (0, 0) -- f is zero.
      Just (i, a) -> -- f is non-zero.
        if i < j then (0, f)
          else
            let m = fromList [(i - j, a / b)]
                (s, r) = euclideanDivision (f - m * g) g
            in (s + m, r)

-- Given polynomials f and g, not both zero, returns a list of signed remainders
-- such that polynomials such that when both f and g are non-zero, then the last
-- element of the returned list is gcd(f, g).
signedRemainderSequence :: Polynomial Rational -> Polynomial Rational -> [Polynomial Rational]
signedRemainderSequence 0 0 = error "Invalid input to signed remainder sequence."
signedRemainderSequence f 0 = [f]
signedRemainderSequence f g =
  let (_, r) = euclideanDivision f g
  in f : signedRemainderSequence g (-r)

-- Given polynomials f and g, returns a list of triples of polynomials such that
-- the last element (d, u, v) satisfies the equation
-- d = u * f + v * g (Bézout's identity) with d = gcd(f, g) and
-- deg(u) < deg(g) - deg(d) and deg(v) < deg(f) - deg(d).
extendedSignedRemainderSequence :: Polynomial Rational -> Polynomial Rational -> [(Polynomial Rational, Polynomial Rational, Polynomial Rational)]
extendedSignedRemainderSequence f g = recurrence (f, 1, 0) (g, 0, 1)
  where
    recurrence :: (Polynomial Rational, Polynomial Rational, Polynomial Rational) -> (Polynomial Rational, Polynomial Rational, Polynomial Rational) -> [(Polynomial Rational, Polynomial Rational, Polynomial Rational)]
    recurrence (s0@(r0, u0, v0)) (s1@(r1, u1, v1)) = s0 : do
        guard $ r1 /= 0
        let (a2, _) = euclideanDivision r0 r1
        recurrence s1 (-r0 + a2 * r1, -u0 + a2 * u1, -v0 + a2 * v1)
