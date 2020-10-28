{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AlgebraicNumberTests where

import AlgebraicNumber
import Polynomial (fromList)
import Interval (interval)
import Data.Ratio ((%))
import PolynomialTests()

import Test.Tasty
import Test.Tasty.QuickCheck

genFieldExtension :: Gen (Root Rational)
genFieldExtension = elements [
    root (fromList [(2, 1), (0, -2)]) (interval (141 % 100, 142 % 100)), -- sqrt(2)
    root (fromList [(2, 1), (0, -3)]) (interval (173 % 100, 174 % 100)), -- sqrt(3)
    root (fromList [(4, 1), (2, -10), (0, 1)]) (interval (314 % 100, 315 % 100)), -- sqrt(2) + sqrt(3)
    root (fromList [(3, 8), (2, -4), (1, -4), (0, 1)]) (interval (90 % 100, 91 % 100)) -- cos(pi/7)
  ]

algebraicNumberOfExt :: Root Rational -> Gen AlgebraicNumber
algebraicNumberOfExt r =
  oneof [
    F <$> arbitrary,
    algebraicNumber r <$> arbitrary
  ]

genAlgebraicNumber :: Gen AlgebraicNumber
genAlgebraicNumber = do
  r <- genFieldExtension
  algebraicNumberOfExt r

genAlgebraicNumberPair :: Gen (AlgebraicNumber, AlgebraicNumber)
genAlgebraicNumberPair = do
  r <- genFieldExtension
  x <- algebraicNumberOfExt r
  y <- algebraicNumberOfExt r
  return (x, y)

genAlgebraicNumberTriple :: Gen (AlgebraicNumber, AlgebraicNumber, AlgebraicNumber)
genAlgebraicNumberTriple = do
  r <- genFieldExtension
  x <- algebraicNumberOfExt r
  y <- algebraicNumberOfExt r
  z <- algebraicNumberOfExt r
  return (x, y, z)

-- instance of Ord

prop_ord_reflexivity :: Property
prop_ord_reflexivity =
  forAll genAlgebraicNumber $ \x -> x <= x

prop_ord_antisymmetry :: Property
prop_ord_antisymmetry = -- We require the contrapositive.
  forAll genAlgebraicNumberPair $ \(x, y) -> x /= y ==> x > y || y > x

prop_ord_transitivity :: Property
prop_ord_transitivity =
  forAll genAlgebraicNumberTriple $ \(x, y, z) -> x <= y && y <= z ==> x <= z

-- instance of Num

prop_add_neutral_elm :: Property
prop_add_neutral_elm =
  forAll genAlgebraicNumber $ \x -> x + 0 == x

prop_add_associative :: Property
prop_add_associative =
  forAll genAlgebraicNumberTriple $ \(x, y, z) -> (x + y) + z == x + (y + z)

prop_add_commutative :: Property
prop_add_commutative =
  forAll genAlgebraicNumberPair $ \(x, y) -> x + y == y + x

prop_add_inverse :: Property
prop_add_inverse =
  forAll genAlgebraicNumber $ \x -> x - x == 0

prop_mul_neutral_elm :: Property
prop_mul_neutral_elm =
  forAll genAlgebraicNumber $ \x -> x * 1 == x

prop_mul_associative :: Property
prop_mul_associative =
  forAll genAlgebraicNumberTriple $ \(x, y, z) -> (x * y) * z == x * (y * z)

prop_mul_commutative :: Property
prop_mul_commutative =
  forAll genAlgebraicNumberPair $ \(x, y) -> x * y == y * x

prop_add_mul_distributivity :: Property
prop_add_mul_distributivity =
  forAll genAlgebraicNumberTriple $ \(x, y, z) ->
    x * (y + z) == x * y + x * z

prop_mul_zero_is_absorbing :: Property
prop_mul_zero_is_absorbing =
  forAll genAlgebraicNumber $ \x -> x * 0 == 0

prop_neg_one_negates_with_mul :: Property
prop_neg_one_negates_with_mul =
  forAll genAlgebraicNumber $ \x -> (-1) * x == -x

prop_neg_twice_is_identity :: Property
prop_neg_twice_is_identity =
  forAll genAlgebraicNumber $ \x -> (-(-x)) == x

prop_abs_signum :: Property
prop_abs_signum =
  forAll genAlgebraicNumber $ \x -> abs x * signum x == x

-- instance of Fractional

prop_mul_inverse :: Property
prop_mul_inverse =
  forAll genAlgebraicNumber $ \x -> x /= 0 ==> x / x == 1

algebraicNumberTests :: TestTree
algebraicNumberTests = testGroup "algebraicNumber" [
    testProperty "ordering is reflexive" prop_ord_reflexivity,
    testProperty "ordering is antisymmetric" prop_ord_antisymmetry,
    testProperty "ordering is transitive" prop_ord_transitivity,
    testProperty "additive neutral element" prop_add_neutral_elm,
    testProperty "addition is associative" prop_add_associative,
    testProperty "addition is commutative" prop_add_commutative,
    testProperty "additive inverse" prop_add_inverse,
    testProperty "multiplicative neutral element" prop_mul_neutral_elm,
    testProperty "multiplication is associative" prop_mul_associative,
    testProperty "multiplication is commutative" prop_mul_commutative,
    testProperty "distributivity" prop_add_mul_distributivity,
    testProperty "multiplication: 0 * x == 0" prop_mul_zero_is_absorbing,
    testProperty "negate: (-1) * x == -x" prop_neg_one_negates_with_mul,
    testProperty "negate: -(-x) == x" prop_neg_twice_is_identity,
    testProperty "|x| * sign(x) == x" prop_abs_signum,
    testProperty "x / x == 1 for x /= 0" prop_mul_inverse
  ]
