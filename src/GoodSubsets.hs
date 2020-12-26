module GoodSubsets (VectorType, VectorTypeSet, recurse, inflate, permutations, rotationsAndReflections) where

import Vector (Vector, zero, (|-|), (|+|), (|*|), dot, unit)
import Matrix (nullSpaceBasis)
import Permutation (permute, symmetricGroup, dihedralGroup)
import Utils (minBy, maxBy, (!), zipPedantic)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), Constraint, constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints)

import qualified Data.Set as Set
import Data.Set (Set, fromList, insert, (\\), elems, empty, union, singleton, intersection)
import Data.List (genericLength, transpose, genericReplicate)
import Control.Monad (guard, foldM)
import Data.Maybe (fromJust)

type VectorType = Vector Integer
type VectorTypeSet = Set VectorType

initialAngleCP :: Integer -> Maybe (ConvexPolytope Rational)
initialAngleCP n = do
  ass <- intersectWithHyperPlane (space n) (HP (genericReplicate n 1) (fromInteger $ n - 2))
  boundedConvexPolytope NonStrict ass [ineq i | i <- [1..n + 1]] -- 1 >= x_1 >= x_2 >= ... >= x_n >= 0
  where
    ineq :: Integer -> Constraint Rational
    ineq i =
      let v = [if j == i - 1 then -1 else if j == i then 1 else 0 | j <- [1..n]]
          q = if i == 1 then 1 else 0
      in constraint v q

minmax :: Integer -> ConvexPolytope Rational -> ([Rational], [Rational], [Vector Rational], [Vector Rational])
minmax n cp =
  let extr = extremePoints cp
      coordLists = transpose $ elems extr
      mins = map minimum coordLists
      maxs = map maximum coordLists
      minXs = [minBy (!i) extr | i <- [1..n]]
      maxXs = [maxBy (!i) extr | i <- [1..n]]
  in (mins, maxs, minXs, maxXs)

initialGoodnessCP :: Integer -> ConvexPolytope Rational
initialGoodnessCP n = fromJust $ do -- The zero vector is always an element of it.
  ass <- intersectWithHyperPlane (space n) (HP (genericReplicate n 1) 0)
  boundedConvexPolytope NonStrict ass [c | i <- [1..n], c <- ineqs i] -- [-1, 1]^n
  where
    ineqs :: Integer -> [Constraint Rational]
    ineqs i =
      let e = unit n i
      in [constraint (zero n |-| e) 1, constraint e 1]

inflate :: Integer -> VectorTypeSet -> Maybe (VectorTypeSet, ConvexPolytope Rational)
inflate n xs =
  do
    ass <- intersectWithHyperPlane (space n) (HP (genericReplicate n 1) (fromInteger $ n - 2))
    cp <- boundedConvexPolytope NonStrict ass [c | i <- [1..n], c <- ineqs i] -- [0, 1]^n
    angleCP <- foldM projectOntoHyperplane cp [HP (asRational v) 2 | v <- elems xs]
    let points = elems $ extremePoints angleCP
    let point = (1 / genericLength points) |*| (foldl (|+|) (zero n) points)
    let compat = compatSet n xs point
    angleCP' <- foldM projectOntoHyperplane cp [HP (asRational v) 2 | v <- elems compat]
    guard $ all (\v -> 0 < v && v < 1) point
    return $ (compat, angleCP')
  where
    ineqs :: Integer -> [Constraint Rational]
    ineqs i =
      let e = unit n i
      in [constraint (zero n |-| e) 0, constraint e 1]

recurse :: Integer -> Set VectorTypeSet
recurse n = recurse' n empty empty (initialAngleCP n) (initialGoodnessCP n)

recurse' :: Integer -> VectorTypeSet -> VectorTypeSet -> Maybe (ConvexPolytope Rational) -> ConvexPolytope Rational -> Set VectorTypeSet
recurse' _ _ _ Nothing _ = empty
recurse' n xs except (Just angleCP) goodnessCP =
  let (minX, maxX, minXpoints, maxXpoints) = minmax n angleCP
  in if any (1<=) minX || any (0>=) maxX then empty else
      let alpha = (1 / 2) |*| (head minXpoints |+| last maxXpoints) -- Pick any 'interior' point.
          compat = compatSet n xs alpha
      in if not . null $ except `intersection` (compat \\ xs) then empty else
          let goodnessCP' = fromJust $ foldM cutHalfSpace goodnessCP [constraint (asRational v) 0 | v <- elems $ compat \\ xs]
              goodSets = if isGood compat goodnessCP' then singleton compat else empty
              u = constructU (zipPedantic minX minXpoints) alpha
              vs = constructV u minX
              except' = compat `union` except
              vs' = elems $ vs \\ except'
          in fst $ foldl (\(goodSets', except'') v ->
              let except''' = insert v except''
                  angleCP' = projectOntoHyperplane angleCP (HP (asRational v) 2)
                  goodnessCP'' = fromJust $ cutHalfSpace goodnessCP' (constraint (asRational v) 0)
                  xs' = insert v compat
              in (goodSets' `union` recurse' n xs' except''' angleCP' goodnessCP'', except''')) (goodSets, except') vs'

asRational :: Vector Integer -> Vector Rational
asRational = fmap fromInteger

compatOrthogonalComplementBasis :: Integer -> VectorTypeSet -> [Vector Rational]
compatOrthogonalComplementBasis n xs =
  nullSpaceBasis $ ((genericReplicate n 1) ++ [fromInteger $ n - 2]) : [asRational x ++ [2] | x <- elems xs]

compatSet :: Integer -> VectorTypeSet -> Vector Rational -> VectorTypeSet
compatSet n xs alpha =
  let orthogonalComplement = compatOrthogonalComplementBasis n xs
      candidates = sequence [[0..floor (2 / a)] | a <- alpha]
      isCompat x = all (\b -> x `dot` b == 0) orthogonalComplement
  in fromList [x | x <- candidates, isCompat (asRational x ++ [2])]

isGood :: VectorTypeSet -> ConvexPolytope Rational -> Bool
isGood compat goodnessCP =
  let extr = extremePoints goodnessCP
  in all (\e -> all (\c -> e `dot` asRational c == 0) compat) extr

constructV :: Vector Rational -> Vector Rational -> VectorTypeSet
constructV u minX =
  let candidates = foldl (\vs (minXi, ui) -> [v' ++ [vi] | v' <- vs, vi <- [0..bound v' minXi ui]]) [[]] $ zipPedantic minX u
  in fromList [v | v <- candidates, asRational v `dot` u >= 0, asRational v `dot` minX <= 2]
  where
    bound v' minXi ui = floor $ if minXi > 0
        then 2 / minXi
        else (-1 / ui) * sum [vj * (max 0 uj) | (vj, uj, minXj) <- zip3 (asRational v') u minX, minXj > 0]

constructU :: [(Rational, Vector Rational)] -> Vector Rational -> Vector Rational
constructU [(minXNMinusOne, minXpointNMinusOne), (minXN, minXpointN)] alpha =
  let alpha' = if minXN == 0 && minXNMinusOne == 0 then minXpointNMinusOne else minXpointN
  in alpha' |-| alpha
constructU (_:mins) alpha' = constructU mins alpha'
constructU [] _ = error "Invalid input."

permutations :: Integer -> VectorTypeSet -> [VectorTypeSet]
permutations n vs = [Set.map (permute p) vs | p <- symmetricGroup n]

rotationsAndReflections :: Integer -> VectorTypeSet -> [VectorTypeSet]
rotationsAndReflections n vs = [Set.map (permute p) vs | p <- dihedralGroup n]
