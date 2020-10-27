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
prop_add_neutral_elm f = add f zeroPolynomial == f

prop_add_associative :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_add_associative f g h = add (add f g) h == add f (add g h)

prop_add_commutative :: Polynomial Integer -> Polynomial Integer -> Bool
prop_add_commutative f g = add f g == add g f

prop_mul_neutral_elm :: Polynomial Integer -> Bool
prop_mul_neutral_elm f = mul (constant 1) f == f

prop_mul_associative :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_mul_associative f g h = mul (mul f g) h == mul f (mul g h)

prop_mul_commutative :: Polynomial Integer -> Polynomial Integer -> Bool
prop_mul_commutative f g = mul f g == mul g f

prop_add_mul_distributivity :: Polynomial Integer -> Polynomial Integer -> Polynomial Integer -> Bool
prop_add_mul_distributivity f g h = mul f (add g h) == add (mul f g) (mul f h)

prop_mul_zero_is_absorbing :: Polynomial Integer -> Bool
prop_mul_zero_is_absorbing f = mul zeroPolynomial f == zeroPolynomial

prop_neg_one_negates_with_mul :: Polynomial Integer -> Bool
prop_neg_one_negates_with_mul f = mul (constant (-1)) f == neg f

prop_neg_twice_is_identity :: Polynomial Integer -> Bool
prop_neg_twice_is_identity f = neg (neg f) == f

prop_sub_then_add_is_identity :: Polynomial Integer -> Polynomial Integer -> Bool
prop_sub_then_add_is_identity f g = add (sub f g) g == f

prop_sub_flipping_args_negates :: Polynomial Integer -> Polynomial Integer -> Bool
prop_sub_flipping_args_negates f g = sub f g == neg (sub g f)

prop_degree_add_does_not_increase_degree :: Polynomial Integer -> Polynomial Integer -> Bool
prop_degree_add_does_not_increase_degree f g = degree (add f g) <= max (degree f) (degree g)

prop_degree_mul_adds_degrees :: Polynomial Integer -> Polynomial Integer -> Property
prop_degree_mul_adds_degrees f g =
  f /= zeroPolynomial && g /= zeroPolynomial
    ==> degree (mul f g) == degree f + degree g

prop_derivative_decrements_degree :: Polynomial Integer -> Property
prop_derivative_decrements_degree f =
  f /= zeroPolynomial ==> degree (derivative f) == degree f - 1

prop_bound_bounds_eval :: Polynomial Rational -> Interval Rational -> Bool
prop_bound_bounds_eval f i =
  let b = bound f i
  in all (isElementOf b) [evaluate f x | x <- [begin i, end i, midpoint i]]

prop_euclidean_division :: Polynomial Rational -> Polynomial Rational -> Property
prop_euclidean_division f g =
  g /= zeroPolynomial ==>
    let (q, r) = euclideanDivision f g
    in add (mul q g) r == f && degree r < degree g

polynomialTests :: TestTree
polynomialTests = testGroup "polynomial" [
    testProperty "add: neutral element" prop_add_neutral_elm,
    testProperty "add: associative" prop_add_associative,
    testProperty "add: commutative" prop_add_commutative,
    testProperty "mul: neutral element" prop_mul_neutral_elm,
    testProperty "mul: associative" prop_mul_associative,
    testProperty "mul: commutative" prop_mul_commutative,
    testProperty "add, mul: distributivity" prop_add_mul_distributivity,
    testProperty "mul: zero is absorbing" prop_mul_zero_is_absorbing,
    testProperty "neg: mul with (neg 1) negates" prop_neg_one_negates_with_mul,
    testProperty "neg: twice is identity" prop_neg_twice_is_identity,
    testProperty "sub: sub then add same is identity" prop_sub_then_add_is_identity,
    testProperty "sub: flipping args negates" prop_sub_flipping_args_negates,
    testProperty "degree: add does not increase degrees" prop_degree_add_does_not_increase_degree,
    testProperty "degree: degree of mul is sum of degrees" prop_degree_mul_adds_degrees,
    testProperty "derivative: decrements degree" prop_derivative_decrements_degree,
    testProperty "bound: bounds evaluations at endpoints and midpoint" prop_bound_bounds_eval,
    testProperty "euclideanDivision: f == g * q + r, deg r < deg g" prop_euclidean_division
  ]
