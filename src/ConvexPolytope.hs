module ConvexPolytope (ConvexPolytope, Strictness(..), Constraint, constraint, boundedConvexPolytope, cutHalfSpace, cutHyperplane, extremePoints, localExtremePoints, fromRationalConvexPolytope, affineSubspace) where

import Vector (Vector, zero, (|-|), (|*|), dot, isZero)
import Matrix (rank)
import AffineSubspace (AffineSubspace (..), Hyperplane (..), coordsInSpace, dimension, space, intersectWithHyperplane, fromRationalAffineSubspace)
import AlgebraicNumber
import JSON

import qualified Data.Set as Set
import Data.Set (Set, elems, fromList)
import Data.List (find, tails, partition)
import Control.Monad (guard)

-- <= or < depending on strictness. Constraints should always be normalised, so
-- the constructor should be considered private.
data Constraint a = Constraint (Vector a) a deriving (Eq, Ord, Show)

data Strictness = NonStrict | Strict deriving (Eq, Show)

-- CP strictness ass cs extr
data ConvexPolytope a = CP Strictness (AffineSubspace a) (Set (Constraint a)) (Set (Vector a)) deriving (Show)

affineSubspace :: ConvexPolytope a -> AffineSubspace a
affineSubspace (CP _ ass _ _) = ass

-- Ensures unique representation of constraints.
constraint :: (Fractional a, Ord a) => Vector a -> a -> Constraint a
constraint vs q =
  case find (/=0) vs of
    (Just v) -> let c = abs (1 / v) in Constraint (c |*| vs) (c * q)
    Nothing -> Constraint vs (signum q)

evaluateConstraint :: (Num a, Ord a) => Constraint a -> Vector a -> Ordering
evaluateConstraint (Constraint vs q) x = (vs `dot` x) `compare` q

satisfiesConstraint :: (Num a, Ord a) => Strictness -> Constraint a -> Vector a -> Bool
satisfiesConstraint Strict  c x = evaluateConstraint c x == LT
satisfiesConstraint NonStrict c x = evaluateConstraint c x /= GT

boundingHyperplane :: Constraint a -> Hyperplane a
boundingHyperplane (Constraint vs q) = HP vs q

projectConstraint :: (Fractional a, Ord a) => AffineSubspace a -> Constraint a -> Constraint a
projectConstraint (ASS p bs _) (Constraint vs q) =
  let vs' = [vs `dot` b | b <- bs]
      q' = q - (vs `dot` p)
  in constraint vs' q'

-- Maintains the invariant if Just extr then extr is non-empty.
-- Should be given projected constraints as input.
computeLocalExtremePoints :: (Fractional a, Ord a) => Integer -> Set (Constraint a) -> Set (Vector a)
computeLocalExtremePoints n pcs =
  let xs = candidates (Just $ space n) (elems pcs)
  in fromList [x | x <- xs, all (\pc -> satisfiesConstraint NonStrict pc x) pcs]
  where
    candidates Nothing _ = [] -- The space is zero-dimensional, i.e. a single point.
    candidates (Just (ASS x [] _)) _ = [x] -- The space is zero-dimensional, i.e. a single point.
    candidates (Just subspace) queue = do
      sublist <- tails queue
      guard $ sublist /= []
      let c:cs' = sublist
      let subspace' = intersectWithHyperplane subspace (boundingHyperplane c)
      candidates subspace' cs'

-- Returns Nothing if a constraint is violated due to projection.
projectedConstraints :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Constraint a) -> Maybe (Set (Constraint a))
projectedConstraints strictness ass cs = do
    let pcs = [projectConstraint ass c | c <- elems cs]
    let (zeroPcs, pcs') = partition (\(Constraint vs _) -> isZero vs) pcs
    let d = dimension ass
    guard $ all (\pc -> satisfiesConstraint strictness pc (zero d)) zeroPcs -- Check if constraint has been violated.
    return $ fromList pcs'

affineHull :: (Fractional a, Ord a) => ConvexPolytope a -> Maybe (ConvexPolytope a)
affineHull cp@(CP _ ass cs extr) =
  case find (\c -> let pc@(Constraint vs _) = projectConstraint ass c in (not $ isZero vs) && all (\e -> evaluateConstraint pc e == EQ) extr) cs of
    Nothing -> return $ facetConstraints cp -- Dimensionality of affine hull can not be reduced.
    Just c -> -- Project everything onto the hyperplane defined by the constraint.
      cutHyperplane cp (boundingHyperplane c)

-- Discards every constraint that does not correspond to a facet.
-- Assumes polytope to be full-dimensional within the given affine subspace.
facetConstraints :: (Fractional a, Ord a) => ConvexPolytope a -> ConvexPolytope a
facetConstraints (CP strictness ass cs extr) =
  CP strictness ass (Set.filter isFacet cs) extr
  where
    d = dimension ass
    isFacet c = let pc = projectConstraint ass c in
      case [e | e <- elems extr, evaluateConstraint pc e == EQ] of
        [] -> False -- Not a facet.
        (p0:ps) -> rank [p |-| p0 | p <- ps] == d - 1

-- The provided constraints should correspond to a bounded polytope.
boundedConvexPolytope :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Constraint a) -> Maybe (ConvexPolytope a)
boundedConvexPolytope strictness ass cs = do
    pcs <- projectedConstraints strictness ass cs
    let extr = computeLocalExtremePoints (dimension ass) pcs
    affineHull (CP strictness ass cs extr)

cutHalfSpace :: (Fractional a, Ord a) => ConvexPolytope a -> Constraint a -> Maybe (ConvexPolytope a)
cutHalfSpace (CP strictness ass cs _) c =
  boundedConvexPolytope strictness ass (Set.insert c cs)

cutHyperplane :: (Fractional a, Ord a) => ConvexPolytope a -> Hyperplane a -> Maybe (ConvexPolytope a)
cutHyperplane (CP strictness ass cs _) hp = do
    ass' <- intersectWithHyperplane ass hp
    boundedConvexPolytope strictness ass' cs

extremePoints :: (Num a, Ord a) => ConvexPolytope a -> Set (Vector a)
extremePoints (CP _ ass _ extr) = Set.map (coordsInSpace ass) extr

localExtremePoints :: (Num a, Ord a) => ConvexPolytope a -> Set (Vector a)
localExtremePoints (CP _ _ _ extr) = extr

fromRationalConvexPolytope :: ConvexPolytope Rational -> ConvexPolytope AlgebraicNumber
fromRationalConvexPolytope (CP strictness ass cs extr) =
  let ass' = fromRationalAffineSubspace ass
      cs' = Set.map fromRationalConstraint cs
      extr' = Set.map (map fromRational) extr
  in CP strictness ass' cs' extr'
  where
    fromRationalConstraint :: Constraint Rational -> Constraint AlgebraicNumber
    fromRationalConstraint (Constraint vs q) =
      Constraint (map fromRational vs) (fromRational q)

instance JSON a => JSON (ConvexPolytope a) where
  toJSON (CP _ ass cs _) =
    jsonObject [
      ("ass", toJSON ass),
      ("cs", toJSON cs)
    ]

instance JSON a => JSON (Constraint a) where
  toJSON (Constraint vs q) =
    jsonObject [
      ("t", toJSON vs),
      ("c", toJSON q)
    ]
