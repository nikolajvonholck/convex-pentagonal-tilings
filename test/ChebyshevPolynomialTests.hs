module ChebyshevPolynomialTests where

import ChebyshevPolynomial
import Polynomial (degree)
import Utils (divisors)

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (genericIndex)

prop_chebyshevT_degree :: Integer -> Bool
prop_chebyshevT_degree z =
  let n = abs z in degree (chebyshevT `genericIndex` n) == n

prop_cosine_poly_extension :: Integer -> Bool
prop_cosine_poly_extension z =
  let n = abs z in chebyshevT `genericIndex` n == cosinePoly n

prop_cosine_poly_even :: Integer -> Bool
prop_cosine_poly_even z = cosinePoly z == cosinePoly (abs z)

-- Property: 2 * T_m(x) * T_n(x) = T_{m + n}(x) + T_{|m − n|}(x).
-- John C. Mason and David Christopher. Handscomb (2003), p. 31.
prop_chebyshevT_product :: Integer -> Integer -> Bool
prop_chebyshevT_product z w = let (m, n) = (abs z, abs w) in
  2 * (chebyshevT `genericIndex` m) * (chebyshevT `genericIndex` n) ==
    (chebyshevT `genericIndex` (m + n)) + (chebyshevT `genericIndex` (abs $ m - n))

prop_cosineMinimalPolynomial_recurrence :: Integer -> Bool
prop_cosineMinimalPolynomial_recurrence z =
  let n = 1 + abs z -- >= 1
      s = n `div` 2
      lhs = chebyshevT `genericIndex` (s + 1) - chebyshevT `genericIndex` (if odd n then s else s - 1)
      rhs = 2^s * product [cosineMinimalPolynomial d | d <- divisors n]
  in lhs == rhs


chebyshevPolynomialTests :: TestTree
chebyshevPolynomialTests = testGroup "chebyshev polynomial" [
    testProperty "degree of can chebyshev polynomial" prop_chebyshevT_degree,
    testProperty "cosinePoly is extension of chebyshevT" prop_cosine_poly_extension,
    testProperty "cosinePoly is even" prop_cosine_poly_even,
    testProperty "chebyshev polynomial product" prop_chebyshevT_product,
    testProperty "cosineMinimalPolynomial recurrence" prop_cosineMinimalPolynomial_recurrence
  ]
