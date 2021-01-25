{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TrigonometryTests where

import Trigonometry
import Interval

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Ratio ((%))

genNonNegativeInteger :: Gen Integer
genNonNegativeInteger = abs <$> arbitrary

genPositiveInteger :: Gen Integer
genPositiveInteger = (+1) <$> genNonNegativeInteger

-- Generates rational x in such that 0 <= x <= |r|.
genNonNegativeBoundedRational :: Rational -> Gen Rational
genNonNegativeBoundedRational r = do
  x <- genNonNegativeInteger
  y <- genPositiveInteger
  let (p, q) = (min x y, max x y) -- 0 <= p/q <= 1
  return $ (abs r) * (p % q)

-- Generates interval of width at most |r|.
genBoundedInterval :: Rational -> Gen (Interval Rational)
genBoundedInterval r = do
  a <- arbitrary
  w <- genNonNegativeBoundedRational (abs r)
  return $ interval (a, a + w)

-- Generates element of the given closed interval.
genElementOfInterval :: Interval Rational -> Gen (Rational)
genElementOfInterval i = do
  o <- genNonNegativeBoundedRational (width i)
  return $ o + begin i

-- Approximation of cosine using Double values.
approxCos' :: Rational -> Rational
approxCos' = toRational . (cos :: Double -> Double) . (*pi) . fromRational

-- Approximation of sine using Double values.
approxSin' :: Rational -> Rational
approxSin' = toRational . (sin :: Double -> Double) . (*pi) . fromRational

-- Check whether cos'(x) is good approximation for cos(x * π).
prop_cosine_is_good_approx :: Property
prop_cosine_is_good_approx =
  forAll arbitrary $
  \(x) -> (approxCos' x) `isElementOf` (cos' x)

prop_cosBound_has_image_of_interval_member :: Property
prop_cosBound_has_image_of_interval_member =
  forAll (do
    i <- genBoundedInterval 4
    x <- genElementOfInterval i
    return (i, x)
  ) $ \(i, x) -> (approxCos' x) `isElementOfInterior` (cosBound' i)

prop_cosBound_not_in_image_is_not_interval_member :: Property
prop_cosBound_not_in_image_is_not_interval_member =
  forAll (do
    i <- genBoundedInterval 2
    x <- genElementOfInterval i
    o <- genNonNegativeBoundedRational 4
    return (i, x + o) -- x + o is 'close' to interval.
  ) $ \(i, x) ->
    not ((approxCos' x) `isElementOfInterior` (cosBound' i)) ==> not (x `isElementOf` i)

prop_sinBound_has_image_of_interval_member :: Property
prop_sinBound_has_image_of_interval_member =
  forAll (do
    i <- genBoundedInterval 4
    x <- genElementOfInterval i
    return (i, x)
  ) $ \(i, x) -> (approxSin' x) `isElementOfInterior` (sinBound' i)

prop_sinBound_not_in_image_is_not_interval_member :: Property
prop_sinBound_not_in_image_is_not_interval_member =
  forAll (do
    i <- genBoundedInterval 2
    x <- genElementOfInterval i
    o <- genNonNegativeBoundedRational 4
    return (i, x + o) -- x + o is 'close' to interval.
  ) $ \(i, x) ->
    not ((approxSin' x) `isElementOfInterior` (sinBound' i)) ==> not (x `isElementOf` i)

trigonometryTests :: TestTree
trigonometryTests = testGroup "trigonometry" [
    testProperty "cosine interval has image of cos" (withMaxSuccess 10000 prop_cosine_is_good_approx),
    testProperty "cosBound has image of interval member" (withMaxSuccess 10000 prop_cosBound_has_image_of_interval_member),
    testProperty "not in cosBound implies not in interval" (withMaxSuccess 1000 prop_cosBound_not_in_image_is_not_interval_member),
    testProperty "sinBound has image of interval member" (withMaxSuccess 10000 prop_sinBound_has_image_of_interval_member),
    testProperty "not in sinBound implies not in interval" (withMaxSuccess 1000 prop_sinBound_not_in_image_is_not_interval_member)
  ]
