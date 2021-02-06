module ChebyshevPolynomial (chebyshevT, chebyshevU, commonDenominator, cosinePoly, sinePoly, cosineFieldExtension) where

import AlgebraicNumber (Root, root)
import Polynomial (Polynomial, fromList, constant, euclideanDivision, derivative, evaluate, signedRemainderSequence)
import Interval (Interval, begin, end)
import Trigonometry (cos')

import Data.Ratio (numerator, denominator, (%))
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
-- We return only the polynomial in cos(theta), omitting the sin(theta) factor.
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
  let s = n `div` 2 -- Rounds down.
      f = constant (1 / (fromInteger 2^s))
      ta = chebyshevT `genericIndex` (s + 1)
      tb = chebyshevT `genericIndex` (if odd n then s else s - 1)
      p = f * (ta - tb)
      q = product [cosineMinimalPolynomial d | d <- divisors n, d < n]
  in fst $ euclideanDivision p q

-- Assumes f to be a non-zero polynomial with f(a), f(b) non-zero.
-- Returns the number of distinct real roots of f in the interval (a, b).
sturm :: Polynomial Rational -> Interval Rational -> Integer
sturm f i =
  let (a, b) = (begin i, end i)
  in
    if f == 0 || evaluate f a == 0 || evaluate f b == 0
      then error "Sturm's Theorem: Invalid input"
      else
        let standardSequence = signedRemainderSequence f (derivative f)
        in signVar [evaluate p a | p <- standardSequence] - signVar [evaluate p b | p <- standardSequence]
  where
    -- Allows zero elements.
    signVar :: [Rational] -> Integer
    signVar = signVar' . filter (/=0) -- Drop any zeros.

    -- Assumes all elements to be non-zero.
    signVar' :: [Rational] -> Integer
    signVar' (a1:a2:as) = signVar' (a2:as) + if a1 * a2 < 0 then 1 else 0
    signVar' _ = 0 -- No sign variations if no or only one element.

cosineFieldExtension :: Integer -> Root Rational
cosineFieldExtension n =
  let f = cosineMinimalPolynomial (2 * n)
      i = cos' (1 % n) -- Bound cos(π/n)
  in if sturm f i /= 1
    then error $ "Failed to isolate root cos(π/n) with n = " ++ show n
    else root f i
