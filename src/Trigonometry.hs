module Trigonometry (precision, cos', cosBound', sinBound') where

import Polynomial (Polynomial, fromList, evaluate)
import Interval (Interval, interval, begin, end, offset, extendWith, leastContainingInterval)

import Data.Ratio ((%))

-- https://www.johndcook.com/blog/2018/05/22/best-approximations-for-pi/
-- 0 < approxPi - π < 10^(-11)
approxPi :: Rational
approxPi = 1146408 % 364913

precision :: Rational
precision = 0.0001 -- Should be strictly greater that 2 times precision of approxPi.

-- Returns closed rational interval of width 2 * precision such that cos(x * π)
-- is in the interior of the interval.
cos' :: Rational -> Interval Rational
cos' x =
  let k = fromInteger . floor $ (x + 1) / 2
      x' = x - 2 * k -- x' in [-1, 1[.
      fx = evaluate cosTaylor (approxPi * x')
  in interval (fx - precision, fx + precision) -- Accounts for error in both input and output.
  where
    taylorTerms :: [(Integer, Rational)] -- Infinite list.
    taylorTerms = iterate (\(i, c) -> (i + 2, (-c) / (fromInteger $ (i + 2) * (i + 1)))) (0, 1)

    -- Returns a taylor polynomial f(x) approximating cos(x * approxPi) for x in
    -- [-1, 1[ such that |f(x * approxPi) - cos(x * approxPi)| < precision / 2
    -- for all x in [-1, 1[.
    cosTaylor :: Polynomial Rational
    cosTaylor =
      fromList $ takeWhile (\(i, c) -> precision / 2 < (abs (c * (approxPi^^i)))) taylorTerms

-- Given rational interval [a, b], returns a rational interval
-- [c, d] such that cos(x * π) is in (c, d) for all x in [a, b].
cosBound' :: Interval Rational -> Interval Rational
cosBound' dom =
  -- We first offset the interval by a multiplum k of 2 such that dom' has
  -- -1 <= begin dom' < 1.
  let k = fromInteger . floor $ (begin dom + 1) / 2
      dom' = offset (-2 * k) dom -- Ensures begin dom' is in [-1, 1).
      (a, b) = (begin dom', end dom')
      (ca, cb) = (cos' a, cos' b)
      i' = leastContainingInterval ca cb
      (mi, ma) = (-1 - 2 * precision, 1 + 2 * precision)
  in
    if -1 <= a && a < 0 then
      if b <= 0 then i' -- cos increasing
      else if b <= 1 then extendWith ma i' -- cos(0) = 1
      else interval (mi, ma) -- 1 < b
    else if 0 <= a && a < 1 then
      if b <= 1 then i' -- cos decreasing
      else if b <= 2 then extendWith mi i' -- cos(π) = -1
      else interval (mi, ma) -- 2 < b
    else error "Impossible: We must have -1 <= a < 1"

-- Given rational interval [a, b], returns a rational interval
-- [c, d] such that sin(x * π) is in (c, d) for all x in [a, b].
sinBound' :: Interval Rational -> Interval Rational
sinBound' = cosBound' . (offset $ -1 / 2)
