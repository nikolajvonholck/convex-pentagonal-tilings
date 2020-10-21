module ConvexPolytope (ConvexPolytope, Strictness(..), condition, boundedConvexPolytope, cutHalfSpace, projectOntoHyperplane, extremePoints) where

import Vector (Vector, zero, (|-|), (|*|), dot, isZero)
import Matrix (rank)
import AffineSubspace (AffineSubspace (..), HyperPlane (..), coordsInSpace, dimension, space, intersectWithHyperPlane)

import qualified Data.Set as Set
import Data.Set (Set, elems, partition, insert, fromList)
import Data.List (find, tails)
import Control.Monad (guard)

-- <= or < depending on strictness. Conditions should always be normalized, so
-- the constructor should be considered private.
data Condition a = Cond (Vector a) a deriving (Eq, Ord, Show)

data Strictness = NonStrict | Strict deriving (Show)

-- CP strictness ass conds extr
data ConvexPolytope a = CP Strictness (AffineSubspace a) (Set (Condition a, Condition a)) (Set (Vector a)) deriving (Show)

condition :: (Fractional a, Ord a) => Vector a -> a -> Condition a
condition vs q =
  case find (/=0) vs of
    (Just v) -> let c = abs (1 / v) in Cond (c |*| vs) (c * q)
    Nothing -> Cond vs (signum q)

evaluateCondition :: (Num a, Ord a) => Condition a -> Vector a -> Ordering
evaluateCondition (Cond vs q) x = (vs `dot` x) `compare` q

satisfiesCondition :: (Num a, Ord a) => Strictness -> Condition a -> Vector a -> Bool
satisfiesCondition Strict  cond x = evaluateCondition cond x == LT
satisfiesCondition NonStrict cond x = evaluateCondition cond x /= GT

projectCondition :: (Fractional a, Ord a) => AffineSubspace a -> Condition a -> Condition a
projectCondition (ASS p bs) (Cond vs q) =
  let vs' = [vs `dot` b | b <- bs]
      q' = q - (p `dot` vs)
  in condition vs' q'

-- Maintains the invariant if Just extr then extr is non-empty.
localExtremePoints :: (Fractional a, Ord a) => AffineSubspace a -> Set (Condition a, Condition a) -> Maybe (Set (Vector a))
localExtremePoints ass conds =
  let d = dimension ass
      projectedConds = Set.map snd conds
      extr = helper projectedConds (space d) (elems projectedConds)
  in if extr == [] then Nothing else Just $ fromList extr
  where
    helper :: (Fractional a, Ord a) => Set (Condition a) -> AffineSubspace a -> [Condition a] -> [Vector a]
    helper pcs (ASS p []) _ = -- The space is zero-dimensional, i.e. a single point.
      [p | all (\pc -> satisfiesCondition NonStrict pc p) pcs]
    helper pcs subspace toCheck =
      concat [ helper pcs subspace' cs | sublist <- tails toCheck,
                 sublist /= [],
                 let (Cond vs q):cs = sublist,
                 let intersection = intersectWithHyperPlane subspace (HP vs q),
                 intersection /= Nothing,
                 let Just subspace' = intersection]

-- Returns Nothing if some condition is violated.
withProjectedConditions :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Condition a) -> Maybe (Set (Condition a, Condition a))
withProjectedConditions strictness ass conds =
  do
    let projectedConds = Set.map (\cond -> (cond, projectCondition ass cond)) conds
    let (zeroConds, newConds) = partition (\(_, (Cond vs _)) -> isZero vs) projectedConds
    let d = dimension ass
    guard $ all (\(_, pc) -> satisfiesCondition strictness pc (zero d)) zeroConds -- Check if condition has been violated.
    return newConds

-- Assumes that extr is non-empty.
reduceDimensionality :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Condition a, Condition a) -> Set (Vector a) -> Maybe (ConvexPolytope a)
reduceDimensionality strictness ass conds extr =
  case find (\(_, pc) -> all (\e -> evaluateCondition pc e == EQ) extr) (elems conds) of
    Nothing -> Just $ CP strictness ass conds extr -- Dimensionality cannot be simplified further.
    Just (Cond vs q, _) -> case strictness of
      Strict -> Nothing -- Solution set is empty.
      NonStrict -> -- Project everything onto the hyperplane defined by the condition.
        do
          -- The following three calls should theoretically always succeed.
          ass' <- intersectWithHyperPlane ass (HP vs q)
          conds' <- withProjectedConditions strictness ass' $ Set.map fst conds
          extr' <- localExtremePoints ass' conds'
          reduceDimensionality strictness ass' conds' extr' -- Recursively simplify.

reduceConditions :: (Fractional a, Ord a) => ConvexPolytope a -> ConvexPolytope a
reduceConditions (CP strictness ass conds extr) =
  let d = dimension ass
      conditionsNeeded = Set.filter (\(_, pc) ->
        let points = Set.filter (\e -> evaluateCondition pc e == EQ) extr
        in case elems points of
          [] -> False
          (p0:ps) -> rank [p |-| p0 | p <- ps] == d - 1) conds
  in CP strictness ass conditionsNeeded extr

boundedConvexPolytope :: (Fractional a, Ord a) => Strictness -> AffineSubspace a -> Set (Condition a) -> Maybe (ConvexPolytope a)
boundedConvexPolytope strictness ass conds = do
    conds' <- withProjectedConditions strictness ass conds
    extr <- localExtremePoints ass conds'
    reduceConditions <$> reduceDimensionality strictness ass conds' extr

-- Condition is assumed normalized.
cutHalfSpace :: (Fractional a, Ord a) => ConvexPolytope a -> Condition a -> Maybe (ConvexPolytope a)
cutHalfSpace (cp@(CP strictness ass conds extr)) cond =
  do
    let pc = projectCondition ass cond
    let evaluations = Set.map (satisfiesCondition strictness pc) extr
    -- Ensure that at least one of the existing extreme points lies in the half space.
    guard (any id evaluations) -- Otherwise solution set is empty.
    if all id evaluations -- The condition is superfluous.
      then return cp
      else do
        -- This is why it is important that cond is normalized to not add the same condition twice.
        let conds' = insert (cond, pc) conds
        extr' <- localExtremePoints ass conds'
        reduceConditions <$> reduceDimensionality strictness ass conds' extr'

projectOntoHyperplane :: (Fractional a, Ord a) => ConvexPolytope a -> HyperPlane a -> Maybe (ConvexPolytope a)
projectOntoHyperplane (cp@(CP strictness ass conds _)) hp =
  do
    ass' <- intersectWithHyperPlane ass hp
    if ass' == ass then return cp -- Projection does not change anything.
    else do
      conds' <- withProjectedConditions strictness ass' (Set.map fst conds)
      extr' <- localExtremePoints ass' conds'
      reduceConditions <$> reduceDimensionality strictness ass' conds' extr'

extremePoints :: (Num a, Ord a) => ConvexPolytope a -> Set (Vector a)
extremePoints (CP _ ass _ extr) = Set.map (coordsInSpace ass) extr
