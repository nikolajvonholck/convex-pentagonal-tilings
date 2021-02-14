module ConvexPolytope (ConvexPolytope, Strictness(..), Constraint, constraint, boundedConvexPolytope, cutHalfSpace, projectOntoHyperplane, extremePoints, localExtremePoints, fromRationalConvexPolytope, affineSubspace) where

import Vector (Vector, zero, (|-|), (|*|), dot, isZero)
import Matrix (rank)
import AffineSubspace (AffineSubspace (..), Hyperplane (..), coordsInSpace, dimension, space, intersectWithHyperplane, fromRationalAffineSubspace)
import AlgebraicNumber
import JSON

import qualified Data.Set as Set
import Data.Set (Set, elems, fromList)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (keysSet, partitionWithKey)
import Data.List (find, tails)
import Control.Monad (guard)
import Data.Maybe (maybeToList, fromJust)

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
computeLocalExtremePoints :: (Fractional a, Ord a) => AffineSubspace a -> Set (Constraint a) -> Maybe (Set (Vector a))
computeLocalExtremePoints ass pcs =
  let extr = candidates (space (dimension ass)) (elems pcs)
  in if extr == [] then Nothing else Just $ fromList extr
  where
    candidates (ASS p [] _) _ = -- The space is zero-dimensional, i.e. a single point.
      [p | all (\pc -> satisfiesConstraint NonStrict pc p) pcs]
    candidates subspace queue = do
      sublist <- tails queue
      guard $ sublist /= []
      let c:cs' = sublist
      subspace' <- maybeToList $ intersectWithHyperplane subspace (boundingHyperplane c)
      candidates subspace' cs'

-- Returns Nothing if a constraint is violated due to projection.
withProjectedConstraints :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Constraint a) -> Maybe (Set (Constraint a), Set (Constraint a))
withProjectedConstraints strictness ass cs =
  do
    let csm = Map.fromList [(projectConstraint ass c, c) | c <- elems cs]
    let (zeroConstraints, csm') = partitionWithKey (\(Constraint vs _) _ -> isZero vs) csm
    let d = dimension ass
    guard $ all (\pc -> satisfiesConstraint strictness pc (zero d)) $ keysSet zeroConstraints -- Check if constraint has been violated.
    return (fromList $ Map.elems csm', keysSet csm')

-- Assumes that extr is non-empty.
reduceDimensionality :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Constraint a) -> Set (Vector a) -> Maybe (ConvexPolytope a)
reduceDimensionality strictness ass cs extr =
  case find (\c -> let pc = projectConstraint ass c in all (\e -> evaluateConstraint pc e == EQ) extr) cs of
    Nothing -> Just $ CP strictness ass cs extr -- Dimensionality of affine hull can not be reduced.
    Just c -> case strictness of
      Strict -> Nothing -- Solution set is empty.
      NonStrict -> -- Project everything onto the hyperplane defined by the constraint.
        fromJust $ do -- The following three calls should theoretically always succeed.
          ass' <- intersectWithHyperplane ass (boundingHyperplane c)
          (cs', pcs) <- withProjectedConstraints strictness ass' cs
          extr' <- computeLocalExtremePoints ass' pcs
          return $ reduceDimensionality strictness ass' cs' extr' -- Recursively simplify.

-- Discards constraints that do not correspond to a facet.
reduceConstraints :: (Fractional a, Ord a) => ConvexPolytope a -> ConvexPolytope a
reduceConstraints (CP strictness ass cs extr) =
  let d = dimension ass
      cs' = Set.filter (\c ->
        let pc = projectConstraint ass c
        in case [e | e <- elems extr, evaluateConstraint pc e == EQ] of
          [] -> False
          (p0:ps) -> rank [p |-| p0 | p <- ps] == d - 1) cs
  in CP strictness ass cs' extr

-- The provided constraints should correspond to a bounded polytope.
boundedConvexPolytope :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> [Constraint a] -> Maybe (ConvexPolytope a)
boundedConvexPolytope strictness ass cs = do
    (cs', pcs) <- withProjectedConstraints strictness ass $ fromList cs
    extr <- computeLocalExtremePoints ass pcs
    cp' <- reduceDimensionality strictness ass cs' extr
    return $ reduceConstraints cp'

cutHalfSpace :: (Fractional a, Ord a) => ConvexPolytope a -> Constraint a -> Maybe (ConvexPolytope a)
cutHalfSpace (CP strictness ass cs _) c =
  do
    (cs', pcs) <- withProjectedConstraints strictness ass (Set.insert c cs)
    extr' <- computeLocalExtremePoints ass pcs
    cp' <- reduceDimensionality strictness ass cs' extr'
    return $ reduceConstraints cp'

projectOntoHyperplane :: (Fractional a, Ord a) => ConvexPolytope a -> Hyperplane a -> Maybe (ConvexPolytope a)
projectOntoHyperplane (CP strictness ass cs _) hp =
  do
    ass' <- intersectWithHyperplane ass hp
    (cs', pcs) <- withProjectedConstraints strictness ass' cs
    extr' <- computeLocalExtremePoints ass' pcs
    cp' <- reduceDimensionality strictness ass' cs' extr'
    return $ reduceConstraints cp'

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
