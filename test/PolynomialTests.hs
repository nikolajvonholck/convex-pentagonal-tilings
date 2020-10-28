{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PolynomialTests where

import Polynomial
import Interval (Interval, begin, end, midpoint, isElementOf)
import IntervalTests()

import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary (Polynomial Integer) where
  arbitrary = polynomial <$> arbitrary

instance Arbitrary (Polynomial Rational) where
  arbitrary = polynomial <$> arbitrary

prop_add_neutral_elm :: Polynomial Integer -> Bool
prop_add_neutral_elm f = f + 0 == f

prop_add_associative :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_add_associative f g h = (f + g) + h == f + (g + h)

prop_add_commutative :: Polynomial Integer -> Polynomial Integer -> Bool
prop_add_commutative f g = f + g == g + f

prop_additive_inverse :: Polynomial Integer -> Bool
prop_additive_inverse f = f - f == 0

prop_mul_neutral_elm :: Polynomial Integer -> Bool
prop_mul_neutral_elm f = 1 * f == f

prop_mul_associative :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_mul_associative f g h = (f * g) * h == f * (g * h)

prop_mul_commutative :: Polynomial Integer -> Polynomial Integer -> Bool
prop_mul_commutative f g = f * g == g * f

prop_add_mul_distributivity :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_add_mul_distributivity f g h = f * (g + h) == f * g + f * h

prop_mul_zero_is_absorbing :: Polynomial Integer -> Bool
prop_mul_zero_is_absorbing f = 0 * f == 0

prop_neg_one_negates_with_mul :: Polynomial Integer -> Bool
prop_neg_one_negates_with_mul f = (-1) * f == -f

prop_neg_twice_is_identity :: Polynomial Integer -> Bool
prop_neg_twice_is_identity f = -(-f) == f

prop_degree_add_does_not_increase_degree :: Polynomial Integer -> Polynomial Integer -> Bool
prop_degree_add_does_not_increase_degree f g = degree (f + g) <= max (degree f) (degree g)

prop_degree_mul_adds_degrees :: Polynomial Integer -> Polynomial Integer -> Property
prop_degree_mul_adds_degrees f g =
  f /= 0 && g /= 0 ==> degree (f * g) == degree f + degree g

prop_derivative_decrements_degree :: Polynomial Integer -> Property
prop_derivative_decrements_degree f =
  f /= 0 ==> degree (derivative f) == degree f - 1

prop_bound_bounds_eval :: Polynomial Rational -> Interval Rational -> Bool
prop_bound_bounds_eval f i =
  let b = bound f i
  in all (isElementOf b) [evaluate f x | x <- [begin i, end i, midpoint i]]

prop_euclidean_division :: Polynomial Rational -> Polynomial Rational -> Property
prop_euclidean_division f g =
  g /= 0 ==>
    let (q, r) = euclideanDivision f g
    in q * g + r == f && degree r < degree g

polynomialTests :: TestTree
polynomialTests = testGroup "polynomial" [
    testProperty "additive neutral element" prop_add_neutral_elm,
    testProperty "addition is associative" prop_add_associative,
    testProperty "addition is commutative" prop_add_commutative,
    testProperty "additive inverse" prop_additive_inverse,
    testProperty "multiplicative neutral element" prop_mul_neutral_elm,
    testProperty "multiplication is associative" prop_mul_associative,
    testProperty "multiplication is commutative" prop_mul_commutative,
    testProperty "distributivity" prop_add_mul_distributivity,
    testProperty "multiplication: 0 * x == 0" prop_mul_zero_is_absorbing,
    testProperty "negate: (-1) * x == -x" prop_neg_one_negates_with_mul,
    testProperty "negate: -(-x) == x" prop_neg_twice_is_identity,
    testProperty "degree: add does not increase degrees" prop_degree_add_does_not_increase_degree,
    testProperty "degree: degree of mul is sum of degrees" prop_degree_mul_adds_degrees,
    testProperty "derivative: decrements degree" prop_derivative_decrements_degree,
    testProperty "bound: contains evaluations at endpoints and midpoint" prop_bound_bounds_eval,
    testProperty "euclideanDivision: f == g * q + r, deg r < deg g" prop_euclidean_division
  ]
