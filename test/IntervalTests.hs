{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IntervalTests where

import Interval

import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary (Interval Rational) where
  arbitrary = do
    a <- arbitrary
    w <- arbitrary
    return $ interval (a, a + abs w)

prop_begin_leq_end :: Interval Rational -> Bool
prop_begin_leq_end i = begin i <= end i

prop_begin_is_elem_of :: Interval Rational -> Bool
prop_begin_is_elem_of i = (begin i) `isElementOf` i

prop_end_is_elem_of :: Interval Rational -> Bool
prop_end_is_elem_of i = end i `isElementOf` i

prop_from_elem_is_elem_of :: Rational -> Bool
prop_from_elem_is_elem_of x = x `isElementOf` (fromElement x)

prop_least_containing_interval_contains :: Interval Rational -> Interval Rational -> Bool
prop_least_containing_interval_contains i j =
  let ij = leastContainingInterval i j
  in all (`isElementOf` ij) [begin i, end i, begin j, end j]

prop_least_containing_interval_same_intervals_unchanged :: Interval Rational -> Bool
prop_least_containing_interval_same_intervals_unchanged i =
  leastContainingInterval i i == i

prop_extend_with_contains :: Interval Rational -> Rational -> Bool
prop_extend_with_contains i x =
  let i' = extendWith x i
  in all (`isElementOf` i') [begin i, end i, x]

prop_extend_with_element_unchanged :: Interval Rational -> Bool
prop_extend_with_element_unchanged i =
  all (==i) [extendWith x i | x <- [begin i, end i, midpoint i]]

prop_width_non_negative :: Interval Rational -> Bool
prop_width_non_negative i = width i >= 0

prop_midpoint_contained :: Interval Rational -> Bool
prop_midpoint_contained i = (midpoint i) `isElementOf` i

prop_offset_forward_backward_is_identity :: Interval Rational -> Rational -> Bool
prop_offset_forward_backward_is_identity i x = offset (-x) (offset x i) == i

prop_offset_zero_is_identity :: Interval Rational -> Bool
prop_offset_zero_is_identity i = offset 0 i == i

prop_offset_maintains_width :: Interval Rational -> Rational -> Bool
prop_offset_maintains_width i x = width i == width (offset x i)

prop_offset_contains_offset_points :: Interval Rational -> Rational -> Bool
prop_offset_contains_offset_points i x =
  let i' = offset x i
  in all (`isElementOf` i') [x + begin i, x + end i, x + midpoint i]

intervalTests :: TestTree
intervalTests = testGroup "interval" [
    testProperty "interval begin is less than or equal to end" prop_begin_leq_end,
    testProperty "begin is an element of interval" prop_begin_is_elem_of,
    testProperty "end is an element of interval" prop_end_is_elem_of,
    testProperty "fromElement contains element" prop_from_elem_is_elem_of,
    testProperty "leastContainingInterval contains end points of intervals" prop_least_containing_interval_contains,
    testProperty "leastContainingInterval unchanged on same intervals" prop_least_containing_interval_same_intervals_unchanged,
    testProperty "extendWith element in interval has no effect" prop_extend_with_element_unchanged,
    testProperty "extendWith contains point and end points of interval" prop_extend_with_contains,
    testProperty "width is non-negative" prop_width_non_negative,
    testProperty "midpoint is element of interval" prop_midpoint_contained,
    testProperty "offset forward and backward is identity" prop_offset_forward_backward_is_identity,
    testProperty "offset by zero is identity" prop_offset_zero_is_identity,
    testProperty "offset maintains width" prop_offset_maintains_width,
    testProperty "offset contains offset end points and mid point" prop_offset_contains_offset_points
  ]
