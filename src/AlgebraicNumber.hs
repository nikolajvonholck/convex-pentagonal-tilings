module AlgebraicNumber (AlgebraicNumber) where

import Polynomial (Polynomial, add, mul, neg, sub, evaluate, bound, euclideanDivision, degreeWithLeadingCoefficient, zeroPolynomial, constant)
import Interval (Interval, interval, begin, end, midpoint)

import Data.List (find)
import Data.Maybe (fromJust)

data AlgebraicNumber = F Rational | An (Root Rational) (Polynomial Rational)

data Root a = Root (Polynomial a) (Interval a) deriving (Eq)

bisections :: (Fractional a, Ord a) => Root a -> [Interval a]
bisections (Root f i) = bisections' f i
  where
    -- Assumes that either f(a) < 0 < f(b) or f(b) < 0 < f(a).
    bisections' :: (Fractional a, Ord a) => Polynomial a -> Interval a -> [Interval a]
    bisections' g i' =
      let (a, b, c) = (begin i', end i', midpoint i')
          (ga, gb, gc) = (evaluate g a, evaluate g b, evaluate g c)
          i'' = case ((ga * gb) `compare` 0, (ga * gc) `compare` 0) of
                (LT, LT) -> interval (ga, gc)
                (LT, GT) -> interval (gc, gb)
                _ -> error "Assumptions for bisections violated."
      in i' : bisections' g i''

-- valueWithPrecision :: Root a -> a
-- valueWithPrecision r a = fromJust $ find (\i -> midpoint i <= a) (bisections r)

convertRational :: Root Rational -> Rational -> AlgebraicNumber
convertRational root x = An root (constant x)

instance Eq AlgebraicNumber where
  F x == F y = x == y
  An r x == F y = An r x == convertRational r y
  F x == An r y = convertRational r x == An r y
  An r1 x == An r2 y =
    if r1 == r2
      then x == y
      else error "Incompatible algebraic numbers."

instance Ord AlgebraicNumber where
  x <= y = x == y || isPositive (y - x)
    where
      isPositive :: AlgebraicNumber -> Bool
      isPositive (F x') = 0 < x'
      isPositive (An r x') =
        let bounds = map (bound x') (bisections r)
            i' = fromJust $ find (\i -> 0 < begin i || end i < 0) bounds
        in 0 < begin i'

instance Num AlgebraicNumber where
  F x + F y = F (x + y)
  An r x + F y = An r x + convertRational r y
  F x + An r y = convertRational r x + An r y
  An r1 x + An r2 y =
    if r1 == r2
      then An r1 (add x y)
      else error "Incompatible algebraic numbers."

  F x * F y = F (x * y)
  An r x * F y = An r x * convertRational r y
  F x * An r y = convertRational r x * An r y
  An (r1@(Root f _)) x * An r2 y =
    if r1 == r2
      then let (_, z) = euclideanDivision (mul x y) f in An r1 z
      else error "Incompatible algebraic numbers."

  negate (F x) = F (negate x)
  negate (An r x) = An r (neg x)

  abs x = if 0 <= x then x else negate x

  signum x = case x `compare` 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

  fromInteger = F . fromInteger

instance Fractional AlgebraicNumber where
  fromRational = F

  recip (F x) = F (recip x)
  recip (An (r@(Root f _)) x) =
    case degreeWithLeadingCoefficient x of
      Nothing -> error "Division by algebraic numbers zero."
      Just (0, c) -> An r (constant $ recip c) -- TODO: needed?
      Just _ -> bezout (f, x) (constant 0, constant 1)
    where
      bezout :: (Polynomial Rational, Polynomial Rational) -> (Polynomial Rational, Polynomial Rational) -> AlgebraicNumber
      bezout (w0, w1) (u0, u1) =
        if w1 == zeroPolynomial then An r u0
        else
          let (q, _) = euclideanDivision w0 w1
              w2 = w0 `sub` (q `mul` w1)
              u2 = u0 `sub` (q `mul` u1)
          in bezout (w1, w2) (u1, u2)
