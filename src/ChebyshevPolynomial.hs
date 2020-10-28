module ChebyshevPolynomial (chebyshevT, chebyshevU, commonDenominator, cosinePoly, sinePoly, cosineFieldExtension) where

import AlgebraicNumber (Root, root)
import Polynomial (Polynomial, fromList, constant, euclideanDivision, derivative, evaluate)
import Interval (Interval, interval, begin, end)

import Data.Ratio (numerator, denominator, approxRational, (%))
import Data.List (genericIndex)

-- Chebyshev polynomials of the first kind: T_0, T_1, ...
chebyshevT :: [Polynomial Rational]
chebyshevT = chebyshevPolys 1 (fromList [(1, 1)])

-- Chebyshev polynomials of the second kind: U_0, U_1, ...
chebyshevU :: [Polynomial Rational]
chebyshevU = chebyshevPolys 1 (fromList [(1, 2)])

-- Follows the recurrence relation F_{n + 1}(x) = 2x * F_n(x) - F_{n - 1}(x).
chebyshevPolys :: Polynomial Rational -> Polynomial Rational -> [Polynomial Rational]
chebyshevPolys a b = a : chebyshevPolys b ((fromList [(1, 2)] * b) - a)

-- Assumes list to be non-empty.
commonDenominator :: [Rational] -> ([Integer], Integer)
commonDenominator [] = error "Can not determine common denominator for empty list."
commonDenominator xs =
  let lcd = foldl1 lcm (map denominator xs) -- Least common denominator.
  in ([numerator (x * (fromInteger lcd)) | x <- xs], lcd)

-- Using that cosine is even and cos(n * theta) = T_n(cos(theta)) for all
-- non-negative integers, then we get for all integers z:
--   cos(z * theta) = T_{|z|}(cos(theta)).
cosinePoly :: Integer -> Polynomial Rational
cosinePoly n = chebyshevT `genericIndex` (abs n)

-- Uses that sine is odd and sin((n + 1) * theta) = U_n(cos(theta)) * sin(theta)
-- for all non-negative integers, then we get for all non-zero integers z:
--   sin(z * theta) = sign(z) * U_{|z| - 1}(cos(theta)) * sin(theta),
-- whereas for z == 0 we have sin(z * theta) = 0 * sin(theta).
-- Note that we return only the polynomial in cos(theta).
sinePoly :: Integer -> Polynomial Rational
sinePoly 0 = 0
sinePoly n = (fromInteger (signum n)) * chebyshevU `genericIndex` ((abs n) - 1)

-- Positive divisors of the positive integer n.
divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

-- TODO: Handle that cos(2 * pi / n) is rational for n/2 = 3.
-- Returns the minimal polynomial of cos(2 * pi / n) for a positive integer n.
cosineMinimalPolynomial :: Integer -> Polynomial Rational
cosineMinimalPolynomial n =
  let s = n `div` 2
      f = constant (1 / (fromInteger 2^s))
      ta = chebyshevT `genericIndex` (s + 1)
      tb = chebyshevT `genericIndex` (if odd n then s else s - 1)
      p = f * (ta - tb)
      q = product [cosineMinimalPolynomial d | d <- divisors n, d < n]
  in fst $ euclideanDivision p q

-- TODO: Cleanup math:
-- minimal polynomail is irreducible.
-- Q is a field of characterristic 0 => it is a perfect field
-- perfect field <=> irreducible polynomials are square-free.
-- => Sturm's theorem can be applied.

-- Uses Sturm's theorem to count the number of roots in (a, b].
-- Assumes that the polynomail f is square-free.
sturm :: Polynomial Rational -> Interval Rational -> Integer
sturm p i =
  let (a, b) = (begin i, end i)
      ps = sturmSequence p
  in signVariations ps a - signVariations ps b
  where
    sturmSequence :: Polynomial Rational -> [Polynomial Rational]
    sturmSequence f = f : (derivative f) : helper f (derivative f)
      where
        helper :: Polynomial Rational -> Polynomial Rational -> [Polynomial Rational]
        helper _ 0 = []
        helper p0 p1 =
          let p2 = negate $ snd $ euclideanDivision p0 p1
          in p2 : helper p1 p2

    signVariations :: [Polynomial Rational] -> Rational -> Integer
    signVariations fs x =
      let fxs = [evaluate f x | f <- fs]
      in helper [signum fx | fx <- fxs, fx /= 0]
        where
          helper :: [Rational] -> Integer
          helper (a:b:ss) = helper (b:ss) + if a == b then 0 else 1
          helper _ = 0

isolateRoot :: Polynomial Rational -> Double -> Interval Rational
isolateRoot f x = refine (1 % 100)
  where
    refine :: Rational -> Interval Rational
    refine eps =
      let i = approxInterval eps
          eps' = eps / 2
      in if sturm f i == 1
        then approxInterval eps' -- Refine one final time.
        else refine eps' -- Increase precision.

    -- Approximates interval [x - eps, x + eps].
    approxInterval :: Rational -> Interval Rational
    approxInterval eps =
      let m = approxRational x (fromRational $ eps / 4) -- Just to be sure..
      in interval (m - eps, m + eps)

cosineFieldExtension :: Integer -> Root Rational
cosineFieldExtension n =
  let f = cosineMinimalPolynomial (2 * n)
      x = cos(pi / (fromInteger n))
  in root f (isolateRoot f x)
