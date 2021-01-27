module ConvexPolytope (ConvexPolytope, Strictness(..), Constraint, constraint, boundedConvexPolytope, cutHalfSpace, projectOntoHyperplane, extremePoints, localExtremePoints, fromRationalConvexPolytope, affineSubspace) where

import Vector (Vector, zero, (|-|), (|*|), dot, isZero)
import Matrix (rank)
import AffineSubspace (AffineSubspace (..), HyperPlane (..), coordsInSpace, dimension, space, intersectWithHyperPlane, fromRationalAffineSubspace)
import AlgebraicNumber
import JSON

import qualified Data.Set as Set
import Data.Set (Set, elems, fromList)
import Data.List (find, tails, partition, nub)
import Control.Monad (guard)
import Data.Maybe (isJust)

-- <= or < depending on strictness. Constraints should always be normalized, so
-- the constructor should be considered private.
data Constraint a = Constraint (Vector a) a deriving (Eq, Ord, Show)

data Strictness = NonStrict | Strict deriving (Eq, Show)

-- CP strictness ass cs extr
data ConvexPolytope a = CP Strictness (AffineSubspace a) [(Constraint a, Constraint a)] (Set (Vector a)) deriving (Show)

instance Eq a => Eq (ConvexPolytope a) where
  (CP s ass _ extr) == (CP s' ass' _ extr') =
    (s, ass, extr) == (s', ass', extr')

affineSubspace :: ConvexPolytope a -> AffineSubspace a
affineSubspace (CP _ ass _ _) = ass

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

projectConstraint :: (Fractional a, Ord a) => AffineSubspace a -> Constraint a -> Constraint a
projectConstraint (ASS p bs _) (Constraint vs q) =
  let vs' = [vs `dot` b | b <- bs]
      q' = q - (p `dot` vs)
  in constraint vs' q'

-- Maintains the invariant if Just extr then extr is non-empty.
computeLocalExtremePoints :: (Fractional a, Ord a) => AffineSubspace a -> [(Constraint a, Constraint a)] -> Maybe (Set (Vector a))
computeLocalExtremePoints ass cs =
  let d = dimension ass
      projectedConstraints = map snd cs
      extr = helper projectedConstraints (space d) projectedConstraints
  in if extr == [] then Nothing else Just $ fromList extr
  where
    helper :: (Fractional a, Ord a) => [Constraint a] -> AffineSubspace a -> [Constraint a] -> [Vector a]
    helper pcs (ASS p [] _) _ = -- The space is zero-dimensional, i.e. a single point.
      [p | all (\pc -> satisfiesConstraint NonStrict pc p) pcs]
    helper pcs subspace toCheck =
      concat [ helper pcs subspace' cs' | sublist <- tails toCheck,
                 sublist /= [],
                 let (Constraint vs q):cs' = sublist,
                 let intersection = intersectWithHyperPlane subspace (HP vs q),
                 isJust intersection,
                 let Just subspace' = intersection]

-- Returns Nothing if some constraint is violated.
withProjectedConstraints :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> [Constraint a] -> Maybe [(Constraint a, Constraint a)]
withProjectedConstraints strictness ass cs =
  do
    let projectedConstraints = [(c, projectConstraint ass c) | c <- cs]
    let (zeroConstraints, newConstraints) = partition (\(_, (Constraint vs _)) -> isZero vs) projectedConstraints
    let d = dimension ass
    guard $ all (\(_, pc) -> satisfiesConstraint strictness pc (zero d)) zeroConstraints -- Check if constraint has been violated.
    return newConstraints

-- Assumes that extr is non-empty.
reduceDimensionality :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> [(Constraint a, Constraint a)] -> Set (Vector a) -> Maybe (ConvexPolytope a)
reduceDimensionality strictness ass cs extr =
  case find (\(_, pc) -> all (\e -> evaluateConstraint pc e == EQ) extr) cs of
    Nothing -> Just $ CP strictness ass cs extr -- Dimensionality cannot be simplified further.
    Just (Constraint vs q, _) -> case strictness of
      Strict -> Nothing -- Solution set is empty.
      NonStrict -> -- Project everything onto the hyperplane defined by the constraint.
        do
          -- The following three calls should theoretically always succeed.
          ass' <- intersectWithHyperPlane ass (HP vs q)
          cs' <- withProjectedConstraints strictness ass' $ map fst cs
          extr' <- computeLocalExtremePoints ass' cs'
          reduceDimensionality strictness ass' cs' extr' -- Recursively simplify.

reduceConstraints :: (Fractional a, Ord a) => ConvexPolytope a -> ConvexPolytope a
reduceConstraints (CP strictness ass cs extr) =
  let d = dimension ass
      constraintsNeeded = filter (\(_, pc) ->
        let points = Set.filter (\e -> evaluateConstraint pc e == EQ) extr
        in case elems points of
          [] -> False
          (p0:ps) -> rank [p |-| p0 | p <- ps] == d - 1) cs
  in CP strictness ass constraintsNeeded extr

boundedConvexPolytope :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> [Constraint a] -> Maybe (ConvexPolytope a)
boundedConvexPolytope strictness ass cs = do
    cs' <- withProjectedConstraints strictness ass $ nub cs -- Here it is important that constraints have been normalized.
    extr <- computeLocalExtremePoints ass cs'
    reduceConstraints <$> reduceDimensionality strictness ass cs' extr

-- Constraint is assumed normalized.
cutHalfSpace :: (Fractional a, Ord a) => ConvexPolytope a -> Constraint a -> Maybe (ConvexPolytope a)
cutHalfSpace (cp@(CP strictness ass cs extr)) c =
  do
    let pc = projectConstraint ass c
    let evaluations = Set.map (satisfiesConstraint strictness pc) extr
    -- Ensure that at least one of the existing extreme points lies in the half space.
    guard (any id evaluations) -- Otherwise solution set is empty.
    if all id evaluations -- The constraint is superfluous.
      then return cp
      else do
        -- This is why it is important that constraint is normalized to not add the same constraint twice.
        let cs' = if any ((pc==) . snd) cs then cs else (c, pc):cs
        extr' <- computeLocalExtremePoints ass cs'
        reduceConstraints <$> reduceDimensionality strictness ass cs' extr'

projectOntoHyperplane :: (Fractional a, Ord a) => ConvexPolytope a -> HyperPlane a -> Maybe (ConvexPolytope a)
projectOntoHyperplane (cp@(CP strictness ass cs _)) hp =
  do
    ass' <- intersectWithHyperPlane ass hp
    if ass' == ass then return cp -- Projection does not change anything.
    else do
      cs' <- withProjectedConstraints strictness ass' (map fst cs)
      extr' <- computeLocalExtremePoints ass' cs'
      reduceConstraints <$> reduceDimensionality strictness ass' cs' extr'

extremePoints :: (Num a, Ord a) => ConvexPolytope a -> Set (Vector a)
extremePoints (CP _ ass _ extr) = Set.map (coordsInSpace ass) extr

localExtremePoints :: (Num a, Ord a) => ConvexPolytope a -> Set (Vector a)
localExtremePoints (CP _ _ _ extr) = extr

fromRationalConvexPolytope :: ConvexPolytope Rational -> ConvexPolytope AlgebraicNumber
fromRationalConvexPolytope (CP strictness ass cs extr) =
  let ass' = fromRationalAffineSubspace ass
      cs' = [(fromRationalConstraint c, fromRationalConstraint cp) | (c, cp) <- cs]
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
      ("cs", toJSON $ map fst cs)
    ]

instance JSON a => JSON (Constraint a) where
  toJSON (Constraint vs q) =
    jsonObject [
      ("t", toJSON vs),
      ("c", toJSON q)
    ]
