module ConvexPolytope (ConvexPolytope, Strictness(..), condition, boundedConvexPolytope, cutHalfSpace, projectOntoHyperplane, extremePoints) where

import Vector (Vector, zero, (|-|), (|*|), dot, isZero)
import Matrix (rank)
import AffineSubspace (AffineSubspace (..), HyperPlane (..), coordsInSpace, dimension, space, intersectWithHyperPlane)

import qualified Data.Set as Set
import Data.Set (Set, elems, partition, insert, fromList)
import Data.List (find, tails)
import Data.Maybe (fromJust)
import Control.Monad (guard)

-- import Debug.Trace (traceShow)

-- <= or < depending on strictness.
-- Conditions should always be normalized.
-- The constructor should be considered private.
data Condition a = Cond (Vector a) a deriving (Eq, Ord, Show)

data Strictness = NonStrict | Strict deriving (Show)

-- CP strictness ass conds extr
data ConvexPolytope a = CP Strictness (AffineSubspace a) (Set (Condition a, Condition a)) (Set (Vector a)) deriving (Show)

condition :: (Fractional a, Ord a, Show a) => Vector a -> a -> Condition a
condition vs q =
  case find (/=0) vs of
    (Just v) -> let c = abs (1 / v) in Cond (c |*| vs) (c * q)
    Nothing -> Cond vs (signum q)

evaluateCondition :: (Num a, Ord a, Show a) => Condition a -> Vector a -> Ordering
evaluateCondition (Cond vs q) x = (vs `dot` x) `compare` q

satisfiesCondition :: (Num a, Ord a, Show a) => Strictness -> Condition a -> Vector a -> Bool
satisfiesCondition Strict  cond x = evaluateCondition cond x == LT
satisfiesCondition NonStrict cond x = evaluateCondition cond x /= GT

projectCondition :: (Fractional a, Ord a, Show a) => AffineSubspace a -> Condition a -> Condition a
projectCondition (ASS p bs) (Cond vs q) =
  let vs' = [vs `dot` b | b <- bs]
      q' = q - (p `dot` vs)
  in condition vs' q'

-- Maintains the invariant if Just extr then extr is non-empty.
localExtremePoints :: (Fractional a, Ord a, Show a) => AffineSubspace a -> Set (Condition a, Condition a) -> Maybe (Set (Vector a))
localExtremePoints ass conds =
  let d = dimension ass
      projectedConds = Set.map snd conds
      extr = helper projectedConds (space d) (elems projectedConds)
  in if extr == [] then Nothing else Just $ fromList extr
  where
    helper :: (Fractional a, Ord a, Show a) => Set (Condition a) -> AffineSubspace a -> [Condition a] -> [Vector a]
    helper pcs (ASS p []) _ = do -- The space is zero-dimensional, i.e. a single point.
        guard $ all (\pc -> satisfiesCondition NonStrict pc p) pcs -- Check that point satisfies constraints non-strictly.
        return p
    helper pcs subspace toCheck = do
      sublist <- tails toCheck
      guard $ sublist /= []
      let (Cond vs q):cs = sublist
      let r = intersectWithHyperPlane subspace (HP vs q)
      guard $ r /= Nothing
      let Just subspace' = r
      helper pcs subspace' cs

-- recomp_hp
-- Returns Nothing if some condition is violated.
withProjectedConditions :: (Fractional a, Ord a, Show a) => Strictness -> AffineSubspace a -> Set (Condition a) -> Maybe (Set (Condition a, Condition a))
withProjectedConditions strictness ass conds =
  do
    let projectedConds = Set.map (\cond -> (cond, projectCondition ass cond)) conds
    let (zeroConds, newConds) = partition (\(_, (Cond vs _)) -> isZero vs) projectedConds
    let d = dimension ass
    guard $ all (\(_, pc) -> satisfiesCondition strictness pc (zero d)) zeroConds -- Check if condition has been violated.
    return newConds

-- Assumes that extr is non-empty.
-- TODO: Do we have more assumptions.
reduceConditions :: (Fractional a, Ord a, Show a) => Strictness -> AffineSubspace a -> Set (Condition a, Condition a) -> Set (Vector a) -> Maybe (ConvexPolytope a)
reduceConditions strictness ass conds extr =
  case find (\(_, pc) -> all (\e -> evaluateCondition pc e == EQ) extr) (elems conds) of
    -- TODO: Consider adding assertion that GT is not encourtered.
    Nothing -> Just $ CP strictness ass conds extr -- Representation cannot be simplified further.
    Just (Cond vs q, _) -> case strictness of -- TODO: Consider rewriting using Maybe monad and do-notation.
      Strict -> Nothing -- Solution set is empty.
      NonStrict -> -- Project everything onto the hyperplane defined by the condition.
          -- The following should theoretically always succeed.
          let ass' = fromJust $ intersectWithHyperPlane ass (HP vs q)
              conds' = fromJust $ withProjectedConditions strictness ass' $ Set.map fst conds
              extr' = fromJust $ localExtremePoints ass' conds'
          in reduceConditions strictness ass' conds' extr' -- Recursively simplify.

-- TODO: Line 517: desc.

reduceConditions2 :: (Fractional a, Ord a, Show a) => ConvexPolytope a -> ConvexPolytope a
reduceConditions2 (CP strictness ass conds extr) =
  let conds' = Set.filter (isNeeded ass extr) conds
  in CP strictness ass conds' extr
  where
    isNeeded :: (Fractional a, Ord a, Show a) => AffineSubspace a -> Set (Vector a) -> (Condition a, Condition a) -> Bool
    isNeeded asss extre (_, pc) =
      let d = dimension asss
          points = Set.filter (\e -> evaluateCondition pc e == EQ) extre
      in case elems points of
        [] -> False
        (p:ps) ->
          let mat = map (|-|p) ps
              rg = rank mat
          in case rg `compare` (d-1) of
            LT -> False
            EQ -> True
            GT -> error "Imp."

--- PUBLIC ---
--- :::::::::::::::::::::: ---
--- :::::::::::::::::::::: ---
--- :::::::::::::::::::::: ---
--- PUBLIC ---

boundedConvexPolytope :: (Fractional a, Ord a, Show a) => Strictness -> AffineSubspace a -> Set (Condition a) -> Maybe (ConvexPolytope a)
boundedConvexPolytope strictness ass conds = do
    conds' <- withProjectedConditions strictness ass conds
    extr <- localExtremePoints ass conds'
    reduced <- reduceConditions strictness ass conds' extr
    return $ reduceConditions2 reduced
    -- TODO: recomp_d (remove more conditions..)

-- Condition is assumed normalized.
-- cut
cutHalfSpace :: (Fractional a, Ord a, Show a) => ConvexPolytope a -> Condition a -> Maybe (ConvexPolytope a)
cutHalfSpace (cp@(CP strictness ass conds extr)) cond =
  do
    let pc = projectCondition ass cond
    let evaluations = Set.map (satisfiesCondition strictness pc) extr
    -- Ensure that at least one of the existing extreme points lies in the half space.
    guard (any id evaluations) -- Otherwise solution set is empty.
    if all id evaluations -- The condition is superfluous.
      then return cp
      else do
        -- TODO: Consider adding assertion that cond is not zero vector.
        -- TODO: Here it is important that cond is normalized to not add the same condition twice.
        -- TODO: Should be impossible to add twice?
        let conds' = insert (cond, pc) conds
        extr' <- localExtremePoints ass conds'
        reduced <- reduceConditions strictness ass conds' extr'
        return $ reduceConditions2 reduced
        -- TODO: recomp_d (remove more conditions..)

-- proj
projectOntoHyperplane :: (Fractional a, Ord a, Show a) => ConvexPolytope a -> HyperPlane a -> Maybe (ConvexPolytope a)
projectOntoHyperplane (CP strictness ass conds _) hp =
  do
    ass' <- intersectWithHyperPlane ass hp
    -- TODO: Consider short curcuit if ass == ass' (Rao check if has same dimensions)
    conds' <- withProjectedConditions strictness ass' (Set.map fst conds)
    extr' <- localExtremePoints ass' conds'
    reduced <- reduceConditions strictness ass' conds' extr'
    return $ reduceConditions2 reduced
    -- TODO: recomp_d (remove more conditions..)

extremePoints :: (Num a, Ord a, Show a) => ConvexPolytope a -> Set (Vector a)
extremePoints (CP _ ass _ extr) = Set.map (coordsInSpace ass) extr
