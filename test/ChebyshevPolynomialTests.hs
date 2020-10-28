{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ChebyshevPolynomialTests where

import ChebyshevPolynomial

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Ratio ((%))

prop_common_denominator_reconstruct :: [Rational] -> Property
prop_common_denominator_reconstruct xs =
  xs /= [] ==> [p % q | let (ps, q) = commonDenominator xs, p <- ps] == xs

chebyshevPolynomialTests :: TestTree
chebyshevPolynomialTests = testGroup "chebyshev polynomial" [
    testProperty "can reconstruct using common denominator" prop_common_denominator_reconstruct
  ]
