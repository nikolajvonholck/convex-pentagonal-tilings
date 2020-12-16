module GoodSubsets (VectorType, VectorTypeSet, recurse, inflate, permutations, rotationsAndReflections) where

import Vector (Vector, zero, (|-|), (|+|), (|*|), dot)
import Matrix (nullSpaceBasis)
import Permutation (permute, symmetricGroup, dihedralGroup)
import Utils (minBy, maxBy, (!))
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints)

import qualified Data.Set as Set
import Data.Set (Set, fromList, insert, (\\), elems, empty, union, singleton, intersection)
import Data.List (genericLength, transpose)
import Control.Monad (guard, foldM)
import Data.Maybe (fromJust)

type VectorType = Vector Integer
type VectorTypeSet = Set VectorType

initialAngleCP :: Maybe (ConvexPolytope Rational)
initialAngleCP = do
  ass <- intersectWithHyperPlane (space 5) (HP [1, 1, 1, 1, 1] 3)
  boundedConvexPolytope NonStrict ass [
      constraint [1, 0, 0, 0, 0] 1,
      constraint [-1, 1, 0, 0, 0] 0,
      constraint [0, -1, 1, 0, 0] 0,
      constraint [0, 0, -1, 1, 0] 0,
      constraint [0, 0, 0, -1, 1] 0,
      constraint [0, 0, 0, 0, -1] 0
    ] -- 1 >= x_1 >= x_2 >= x_3 >= x_4 >= x_5 >= 0

minmax :: ConvexPolytope Rational -> ([Rational], [Rational], [Vector Rational], [Vector Rational])
minmax cp =
  let extr = extremePoints cp
      coordLists = transpose $ elems extr
      mins = map minimum coordLists
      maxs = map maximum coordLists
      minXs = [minBy (!i) extr | i <- [1..5]]
      maxXs = [maxBy (!i) extr | i <- [1..5]]
  in (mins, maxs, minXs, maxXs)

initialGoodnessCP :: ConvexPolytope Rational
initialGoodnessCP = fromJust $ do -- The zero vector is always an element of it.
  ass <- intersectWithHyperPlane (space 5) (HP [1, 1, 1, 1, 1] 0)
  boundedConvexPolytope NonStrict ass [
      constraint [-1, 0, 0, 0, 0] 1, constraint [1, 0, 0, 0, 0] 1,
      constraint [0, -1, 0, 0, 0] 1, constraint [0, 1, 0, 0, 0] 1,
      constraint [0, 0, -1, 0, 0] 1, constraint [0, 0, 1, 0, 0] 1,
      constraint [0, 0, 0, -1, 0] 1, constraint [0, 0, 0, 1, 0] 1,
      constraint [0, 0, 0, 0, -1] 1, constraint [0, 0, 0, 0, 1] 1
    ] -- [-1, 1]^5

inflate :: VectorTypeSet -> Maybe (VectorTypeSet, ConvexPolytope Rational)
inflate xs =
  do
    ass <- intersectWithHyperPlane (space 5) (HP [1, 1, 1, 1, 1] 3)
    cp <- boundedConvexPolytope NonStrict ass [
        constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
        constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
        constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
        constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
        constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
      ] -- [0, 1]^5
    angleCP <- foldM projectOntoHyperplane cp [HP (asRational v) 2 | v <- elems xs]
    let points = elems $ extremePoints angleCP
    let point = (1 / genericLength points) |*| (foldl (|+|) (zero 5) points)
    let compat = compatSet xs point
    angleCP' <- foldM projectOntoHyperplane cp [HP (asRational v) 2 | v <- elems compat]
    guard $ all (\v -> 0 < v && v < 1) point
    return $ (compat, angleCP')

recurse :: Set VectorTypeSet
recurse = recurse' empty empty initialAngleCP initialGoodnessCP

recurse' :: VectorTypeSet -> VectorTypeSet -> Maybe (ConvexPolytope Rational) -> ConvexPolytope Rational -> Set VectorTypeSet
recurse' _ _ Nothing _ = empty
recurse' xs except (Just angleCP) goodnessCP =
  let (minX, maxX, minXpoints, maxXpoints) = minmax angleCP
  in if any (1<=) minX || any (0>=) maxX then empty else
      let alpha = (1 / 2) |*| (head minXpoints |+| last maxXpoints) -- Pick any 'interior' point.
          compat = compatSet xs alpha
      in if not . null $ except `intersection` (compat \\ xs) then empty else
          let goodnessCP' = fromJust $ foldM cutHalfSpace goodnessCP [constraint (asRational v) 0 | v <- elems $ compat \\ xs]
              goodSets = if isGood compat goodnessCP' then singleton compat else empty
              u = constructU minX minXpoints alpha
              vs = constructV u minX
              except' = compat `union` except
              vs' = elems $ vs \\ except'
          in fst $ foldl (\(goodSets', except'') v ->
              let except''' = insert v except''
                  angleCP' = projectOntoHyperplane angleCP (HP (asRational v) 2)
                  goodnessCP'' = fromJust $ cutHalfSpace goodnessCP' (constraint (asRational v) 0)
                  xs' = insert v compat
              in (goodSets' `union` recurse' xs' except''' angleCP' goodnessCP'', except''')) (goodSets, except') vs'

asRational :: Vector Integer -> Vector Rational
asRational = fmap fromInteger

compatOrthogonalComplementBasis :: VectorTypeSet -> [Vector Rational]
compatOrthogonalComplementBasis xs =
  nullSpaceBasis $ [1, 1, 1, 1, 1, 3] : [asRational x ++ [2] | x <- elems xs]

compatSet :: VectorTypeSet -> Vector Rational -> VectorTypeSet
compatSet xs alpha =
  let orthogonalComplement = compatOrthogonalComplementBasis xs
      candidates = sequence [[0..floor (2 / a)] | a <- alpha]
      isCompat x = all (\b -> x `dot` b == 0) orthogonalComplement
  in fromList [x | x <- candidates, isCompat (asRational x ++ [2])]

isGood :: VectorTypeSet -> ConvexPolytope Rational -> Bool
isGood compat goodnessCP =
  let extr = extremePoints goodnessCP
  in not $ any (\e -> any (\c -> e `dot` asRational c < 0) compat) extr

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

permutations :: VectorTypeSet -> [VectorTypeSet]
permutations vs = [Set.map (permute p) vs | p <- s5]
  where
    s5 = symmetricGroup 5

rotationsAndReflections :: VectorTypeSet -> [VectorTypeSet]
rotationsAndReflections vs = [Set.map (permute p) vs | p <- d5n]
  where
    d5n = dihedralGroup 5
