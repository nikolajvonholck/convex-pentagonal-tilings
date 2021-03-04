module ChebyshevPolynomialTests where

import ChebyshevPolynomial
import Polynomial (degree, fromList)
import Utils (divisors)
import Interval (interval)

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List (genericIndex, genericLength, genericReplicate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (keys, toList)

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

prop_sturm_counts_distinct_roots :: Rational -> Rational -> [(Rational, Integer)] -> Bool
prop_sturm_counts_distinct_roots a' b' roots' =
  let (a, b) = (min a' b', max a' b')
      -- Map of roots with multiplicities.
      roots = Map.fromList $ take 5 [(r, mu) | (r, mu') <- roots', r /= a, r /= b, let mu = min 5 (1 + abs mu')]
      count = genericLength $ filter (\r -> a < r && r < b) $ keys roots
      f = product [product $ genericReplicate mu (fromList [(0, -r), (1, 1)]) | (r, mu) <- toList roots]
  in f == 1 || sturm f (interval (a, b)) == count

prop_sturm_counts_all_roots :: [(Rational, Integer)] -> Bool
prop_sturm_counts_all_roots roots' =
  let -- Map of roots with multiplicities.
      roots = Map.fromList $ take 5 [(r, mu) | (r, mu') <- roots', let mu = min 5 (1 + abs mu')]
      distinctRoots = keys roots
      (a, b) = (minimum distinctRoots - 1, maximum distinctRoots + 1) -- Contains all roots.
      count = genericLength distinctRoots
      f = product [product $ genericReplicate mu (fromList [(0, -r), (1, 1)]) | (r, mu) <- toList roots]
  in f == 1 || sturm f (interval (a, b)) == count

prop_sturm_counts_no_roots_if_none :: [(Rational, Integer)] -> Bool
prop_sturm_counts_no_roots_if_none roots' =
  let -- Map of roots with multiplicities.
      roots = Map.fromList $ take 5 [(r, mu) | (r, mu') <- roots', let mu = min 5 (1 + abs mu')]
      distinctRoots = keys roots
      (a, b) = (minimum distinctRoots - 2, minimum distinctRoots - 1) -- Contains all roots.
      f = product [product $ genericReplicate mu (fromList [(0, -r), (1, 1)]) | (r, mu) <- toList roots]
  in f == 1 || sturm f (interval (a, b)) == 0

chebyshevPolynomialTests :: TestTree
chebyshevPolynomialTests = testGroup "chebyshev polynomial" [
    testProperty "degree of can chebyshev polynomial" prop_chebyshevT_degree,
    testProperty "cosinePoly is extension of chebyshevT" prop_cosine_poly_extension,
    testProperty "cosinePoly is even" prop_cosine_poly_even,
    testProperty "chebyshev polynomial product" prop_chebyshevT_product,
    testProperty "cosineMinimalPolynomial recurrence" prop_cosineMinimalPolynomial_recurrence,
    testProperty "sturm counts distinct roots" prop_sturm_counts_distinct_roots,
    testProperty "sturm counts all roots" prop_sturm_counts_all_roots,
    testProperty "sturm counts no roots if none" prop_sturm_counts_no_roots_if_none
  ]
