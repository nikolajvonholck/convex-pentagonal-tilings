module GoodSubsets (recurse) where

import Simplex (Objective(..), Constraint(..), Solution(..), optimize)
import Vector (Vector, zero, unit, (|+|), (|-|), (|*|), dot)
import Matrix (nullSpaceBasis)
import Data.Set (Set, toList, fromList, insert, (\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, member)

type VectorType = Vector Integer
type VectorTypeSet = Set VectorType

recurse :: VectorTypeSet -> Map VectorTypeSet Bool -> Map VectorTypeSet Bool
recurse xs maximalSets =
  case findProperAngles xs of
    Nothing -> maximalSets
    Just alpha ->
      let compat = compatSet xs alpha
      in if member compat maximalSets then maximalSets
        else
          let maximalSets' = Map.insert compat (isGood compat) maximalSets
              (minX, minXpoints) = unzip $ minXs compat
              u = constructU minX minXpoints alpha
              vs = constructV u minX
              toCheck = toList (vs \\ compat)
          in foldl (\ms v -> recurse (insert v xs) ms) maximalSets' toCheck

asRational :: Vector Integer -> Vector Rational
asRational = fmap fromInteger

findProperAngles :: VectorTypeSet -> Maybe (Vector Rational)
findProperAngles xs =
  case searchPolytype xs (Maximize (unit 5 5)) of
    Infeasible -> Nothing
    Unbounded -> error "Polytope is unbounded."
    Optimal (maxX5, alpha1) -> if maxX5 == 0 then Nothing else
      case searchPolytype xs (Minimize (unit 5 1)) of
        Infeasible -> error "Polytope is empty."
        Unbounded -> error "Polytope is unbounded."
        Optimal (minX1, alpha2) -> if minX1 == 1 then Nothing
          else Just $ (1 / 2) |*| (alpha1 |+| alpha2) -- Is element of ]0,1[^5.

compatOrthogonalComplementBasis :: VectorTypeSet -> [Vector Rational]
compatOrthogonalComplementBasis xs =
  nullSpaceBasis $ [1, 1, 1, 1, 1, 3] : [asRational x ++ [2] | x <- toList xs]

compatSet :: VectorTypeSet -> Vector Rational -> VectorTypeSet
compatSet xs alpha =
  let orthogonalComplement = compatOrthogonalComplementBasis xs
      candidates = sequence [[0..floor (2 / a)] | a <- alpha]
      isCompat x = all (\b -> x `dot` b == 0) orthogonalComplement
  in fromList [x | x <- candidates, isCompat (asRational x ++ [2])]

isGood :: VectorTypeSet -> Bool
isGood vs = -- TODO: Improve implementation of non-negetativity constraints.
  let obj = Maximize (zero (5*2))
      ext as = concat [[a, -a] | a <- as]
      commonConstraints = (ext [1, 1, 1, 1, 1] :==: 0) : [ext (asRational w) :>=: 0 | w <- toList vs]
      searches = [optimize obj $ (ext (asRational v) :>=: 1) : commonConstraints | v <- toList vs]
  in all (==Infeasible) searches

constructV :: Vector Rational -> Vector Rational -> VectorTypeSet
constructV (u@[u1, u2, u3, u4, u5]) (minX@[minX1, minX2, minX3, minX4, minX5]) =
  let bound v minXi ui = floor $ if minXi > 0
      then 2 / minXi
      else (-1 / ui) * sum [vj * (max 0 uj) | (vj, uj, minXj) <- zip3 (asRational v) u minX, minXj > 0]
  in fromList [v | v1 <- [0..bound [] minX1 u1],
          v2 <- [0..bound [v1] minX2 u2],
          v3 <- [0..bound [v1, v2] minX3 u3],
          v4 <- [0..bound [v1, v2, v3] minX4 u4],
          v5 <- [0..bound [v1, v2, v3, v4] minX5 u5],
          let v = [v1, v2, v3, v4, v5],
          (asRational v) `dot` u >= 0,
          (asRational v) `dot` minX <= 2]
constructV _ _ = error "Invalid input."

constructU :: Vector Rational -> [Vector Rational] -> Vector Rational -> Vector Rational
constructU [_, _, _, minX4, minX5] [_, _, _, minXpoint4, minXpoint5] alpha' =
  let alpha = if minX5 > 0 || minX4 > 0 then minXpoint5 else minXpoint4
  in alpha |-| alpha'
constructU _ _ _ = error "Invalid input."

minXs :: VectorTypeSet -> [(Rational, Vector Rational)]
minXs xs = do
  i <- [1..5]
  return $ case searchPolytype xs (Minimize (unit 5 i)) of
    Infeasible -> error "Polytope is empty."
    Unbounded -> error "Polytope is unbounded."
    Optimal (value, point) -> (value, point)

searchPolytype :: VectorTypeSet -> Objective -> Solution
searchPolytype xs obj = optimize obj $ polytopeConstraints xs

polytopeConstraints :: VectorTypeSet -> [Constraint]
polytopeConstraints vs = [
    [1, 1, 1, 1, 1] :==: 3,
    [1, 0, 0, 0, 0] :<=: 1,
    [1, -1, 0, 0, 0] :>=: 0,
    [0, 1, -1, 0, 0] :>=: 0,
    [0, 0, 1, -1, 0] :>=: 0,
    [0, 0, 0, 1, -1] :>=: 0
  ] ++ [asRational v :==: 2 | v <- toList vs]
