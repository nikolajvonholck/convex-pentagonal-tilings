module AlgebraicNumber where

import Polynomial (Polynomial, evaluate, boundPolynomial, euclideanDivision, degreeWithLeadingCoefficient, constant, extendedSignedRemainderSequence)
import Interval (Interval, interval, begin, end, midpoint, width, fromElement, isElementOf)

data AlgebraicNumber = F Rational | An (Root Rational) (Polynomial Rational) deriving (Show)

data Root a = Root (Polynomial a) (Interval a) deriving (Show, Eq)

root :: (Fractional a, Ord a) => Polynomial a -> Interval a -> Root a
root f i =
  if evaluate f (begin i) * evaluate f (end i) < 0 || (begin i == end i && evaluate f (begin i) == 0)
    then Root f i
    else error "Invalid isolating interval."

-- Infinite list of bisections of an isolating interval.
bisections :: (Fractional a, Ord a) => Root a -> [Interval a]
bisections (Root f i) = iterate (bisectIsolatingInterval f) i

-- Assumes that f(a) < 0 < f(b), f(b) < 0 < f(a) or a = b and f(a) = f(b) = 0.
-- Returns the first or second half of the given interval such that this
-- property is maintained.
bisectIsolatingInterval :: (Fractional a, Ord a) => Polynomial a -> Interval a -> Interval a
bisectIsolatingInterval f i =
  let (a, m, b) = (begin i, midpoint i, end i)
      (fa, fm, fb) = (evaluate f a, evaluate f m, evaluate f b)
  in if fa * fm < 0 then interval (a, m)
     else if fm * fb < 0 then interval (m, b)
       else fromElement m -- Found rational root.

algebraicNumber :: Root Rational -> Polynomial Rational -> AlgebraicNumber
algebraicNumber (r@(Root f _)) g = An r (snd $ euclideanDivision g f)

inclusion :: Root Rational -> Rational -> AlgebraicNumber
inclusion r x = An r (constant x)

instance Eq AlgebraicNumber where
  F x == F y = x == y
  An r x == F y = An r x == inclusion r y
  F x == An r y = inclusion r x == An r y
  An r1 x == An r2 y =
    if r1 == r2
      then x == y
      else error "Incompatible algebraic numbers."

instance Ord AlgebraicNumber where
  x `compare` y = if x == y then EQ else if isPositive (y - x) then LT else GT
    where
      isPositive :: AlgebraicNumber -> Bool
      isPositive (F x') = 0 < x'
      isPositive (An r x') =
        let bounds = boundPolynomial x' <$> bisections r
            i = head $ dropWhile (0 `isElementOf` i) bounds
        in 0 < begin i

instance Num AlgebraicNumber where
  F x + F y = F (x + y)
  An r x + F y = An r x + inclusion r y
  F x + An r y = inclusion r x + An r y
  An r1 x + An r2 y =
    if r1 == r2
      then An r1 (x + y)
      else error "Incompatible algebraic numbers."

  F x * F y = F (x * y)
  An r x * F y = An r x * inclusion r y
  F x * An r y = inclusion r x * An r y
  An (r1@(Root f _)) x * An r2 y =
    if r1 == r2
      then An r1 (snd $ euclideanDivision (x * y) f)
      else error "Incompatible algebraic numbers."

  negate (F x) = F (negate x)
  negate (An r x) = An r (negate x)

  abs x = if 0 <= x then x else negate x

  signum x = case x `compare` 0 of
    LT -> -1
    EQ -> 0
    GT -> 1

  fromInteger = F . fromInteger

instance Fractional AlgebraicNumber where
  fromRational = F

  recip (F x) = F (recip x)
  recip (An _ 0) = error "Division by algebraic number zero."
  recip (An (r@(Root f _)) x) =
    let (d, u, _) = last $ extendedSignedRemainderSequence x f -- u * x + v * f = d (Bézout's identity)
    in case degreeWithLeadingCoefficient d of -- x * (u / d) = 1 (mod f)
      Just (0, d') -> An r $ constant (recip d') * u -- We have deg(u) < deg(f)
      _ -> error "Impossible. Polynomial f should be relatively prime to x."

approximate :: Rational -> AlgebraicNumber -> Rational
approximate _ (F x) = x
approximate precision (An r x) =
  let bounds = boundPolynomial x <$> bisections r
  in midpoint $ head $ dropWhile (\i -> width i >= precision) bounds
