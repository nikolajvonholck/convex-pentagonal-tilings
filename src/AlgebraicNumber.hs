module AlgebraicNumber where

import Polynomial (Polynomial, evaluate, bound, euclideanDivision, degreeWithLeadingCoefficient, constant)
import Interval (Interval, interval, begin, end, midpoint)

import Data.List (find)
import Data.Maybe (fromJust)

data AlgebraicNumber = F Rational | An (Root Rational) (Polynomial Rational) deriving (Show)

data Root a = Root (Polynomial a) (Interval a) deriving (Show, Eq)

root :: (Fractional a, Ord a) => Polynomial a -> Interval a -> Root a
root f i =
  if evaluate f (begin i) * evaluate f (end i) < 0
    then Root f i
    else error "Invalid root."

bisections :: (Fractional a, Ord a) => Root a -> [Interval a]
bisections (Root f i) = bisections' f i
  where
    -- Assumes that either f(a) < 0 < f(b) or f(b) < 0 < f(a).
    bisections' :: (Fractional a, Ord a) => Polynomial a -> Interval a -> [Interval a]
    bisections' g i' =
      let (a, b, c) = (begin i', end i', midpoint i')
          (ga, gb, gc) = (evaluate g a, evaluate g b, evaluate g c)
          i'' = case ((ga * gb) `compare` 0, (ga * gc) `compare` 0) of
                (LT, LT) -> interval (a, c)
                (LT, GT) -> interval (c, b)
                _ -> error "Assumptions for bisections violated."
      in i' : bisections' g i''

algebraicNumber :: Root Rational -> Polynomial Rational -> AlgebraicNumber
algebraicNumber (r@(Root f _)) g = An r (snd $ euclideanDivision g f)

inclusion :: Root Rational -> Rational -> AlgebraicNumber
inclusion r x = An r (constant x)

instance Eq AlgebraicNumber where
  F x == F y = x == y
  An r x == F y = An r x == inclusion r y
  F x == An r y = inclusion r x == An r y
  An r1 x == An r2 y =
    if r1 == r2
      then x == y
      else error "Incompatible algebraic numbers."

instance Ord AlgebraicNumber where
  x `compare` y = if x == y then EQ else if isPositive (y - x) then LT else GT
    where
      isPositive :: AlgebraicNumber -> Bool
      isPositive (F x') = 0 < x'
      isPositive (An r x') =
        let bounds = map (bound x') (bisections r)
            i' = fromJust $ find (\i -> 0 < begin i || end i < 0) bounds
        in 0 < begin i'

instance Num AlgebraicNumber where
  F x + F y = F (x + y)
  An r x + F y = An r x + inclusion r y
  F x + An r y = inclusion r x + An r y
  An r1 x + An r2 y =
    if r1 == r2
      then An r1 (x + y)
      else error "Incompatible algebraic numbers."

  F x * F y = F (x * y)
  An r x * F y = An r x * inclusion r y
  F x * An r y = inclusion r x * An r y
  An (r1@(Root f _)) x * An r2 y =
    if r1 == r2
      then An r1 (snd $ euclideanDivision (x * y) f)
      else error "Incompatible algebraic numbers."

  negate (F x) = F (negate x)
  negate (An r x) = An r (negate x)

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
      Nothing -> error "Division by algebraic number zero."
      Just (0, c) -> An r (constant $ recip c) -- TODO: Is this only needed for cursive call? Check math!
      Just _ -> bezout (f, x) (constant 0, constant 1)
    where
      bezout :: (Polynomial Rational, Polynomial Rational) -> (Polynomial Rational, Polynomial Rational) -> AlgebraicNumber
      bezout (r0, r1) (t0, t1) =
        if r1 == 0 then (An r t0) * (recip $ An r r0)
        else
          let q = fst $ euclideanDivision r0 r1
          in bezout (r1, r0 - q * r1) (t1, t0 - q * t1)
