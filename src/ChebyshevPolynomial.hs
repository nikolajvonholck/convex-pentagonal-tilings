module ChebyshevPolynomial (chebyshevT, chebyshevU, commonDenominator, cosinePoly, sinePoly) where

import Polynomial (Polynomial, fromList)
import Data.Ratio (numerator, denominator)
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
