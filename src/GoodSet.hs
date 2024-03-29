module GoodSet (VertexType, VertexTypeSet, goodSets, inflate, permutations, ignoringSymmetries, partitionByDimensionality, rotationsAndReflections) where

import Vector (Vector, zero, (|-|), (|+|), (|*|), dot, unit)
import Matrix (nullSpaceBasis)
import Permutation (permute, symmetricGroup, dihedralGroup)
import Utils (minBy, maxBy, (!), zipPedantic)
import AffineSubspace (Hyperplane(..), intersectWithHyperplane, space, dimension)
import ConvexPolytope (ConvexPolytope, Strictness(..), Constraint, constraint, affineSubspace, boundedConvexPolytope, cutHyperplane, cutHalfSpace, extremePoints)

import qualified Data.Set as Set
import Data.Set (Set, fromList, insert, (\\), member, elems, empty, union, unions, intersection)
import qualified Data.Map as Map
import Data.Map (toAscList)
import Data.List (genericLength, transpose, genericReplicate, tails, inits)
import Control.Monad (guard, foldM)
import Data.Maybe (fromJust)

type VertexType = Vector Integer
type VertexTypeSet = Set VertexType

initialAngleCP :: Integer -> Maybe (ConvexPolytope Rational)
initialAngleCP n = do
  ass <- intersectWithHyperplane (space n) (HP (genericReplicate n 1) (fromInteger $ n - 2))
  boundedConvexPolytope NonStrict ass $ fromList [ineq i | i <- [1..n + 1]] -- 0 <= x_1 <= x_2 <= ... <= x_n <= 1
  where
    ineq :: Integer -> Constraint Rational
    ineq i =
      let v = [if j == i - 1 then 1 else if j == i then -1 else 0 | j <- [1..n]]
          q = if i == n + 1 then 1 else 0
      in constraint v q

minmax :: Integer -> ConvexPolytope Rational -> ([Rational], [Rational], [Vector Rational], [Vector Rational])
minmax n cp =
  let es = extremePoints cp
      coordLists = transpose $ elems es
      mins = map minimum coordLists
      maxs = map maximum coordLists
      minEs = [minBy (!i) es | i <- [1..n]]
      maxEs = [maxBy (!i) es | i <- [1..n]]
  in (mins, maxs, minEs, maxEs)

initialGoodnessCP :: Integer -> ConvexPolytope Rational
initialGoodnessCP n = fromJust $ do -- The zero vector is always an element of it.
  ass <- intersectWithHyperplane (space n) (HP (genericReplicate n 1) 0) -- sum_{i = 1}^n x_i = 0
  boundedConvexPolytope NonStrict ass $ fromList [c | i <- [1..n], c <- ineqs i] -- [-1, 1]^n
  where
    ineqs :: Integer -> [Constraint Rational]
    ineqs i =
      let e = unit n i
      in [constraint (zero n |-| e) 1, constraint e 1]

inflate :: Integer -> VertexTypeSet -> Maybe (VertexTypeSet, ConvexPolytope Rational)
inflate n xs =
  do
    ass <- intersectWithHyperplane (space n) (HP (genericReplicate n 1) (fromInteger $ n - 2))
    cp <- boundedConvexPolytope NonStrict ass $ fromList [c | i <- [1..n], c <- ineqs i] -- [0, 1]^n
    angleCP <- foldM cutHyperplane cp [HP (asRational v) 2 | v <- elems xs]
    let es = elems $ extremePoints angleCP
    let as = (1 / genericLength es) |*| (foldl (|+|) (zero n) es)
    let xs' = compat n xs as
    angleCP' <- foldM cutHyperplane cp [HP (asRational v) 2 | v <- elems xs']
    guard $ all (\v -> 0 < v && v < 1) as
    return $ (xs', angleCP')
  where
    ineqs :: Integer -> [Constraint Rational]
    ineqs i =
      let e = unit n i
      in [constraint (zero n |-| e) 0, constraint e 1]

goodSets :: Integer -> Set VertexTypeSet
goodSets n = goodSetsExcluding n empty empty (initialAngleCP n) (initialGoodnessCP n)

goodSetsExcluding :: Integer -> VertexTypeSet -> VertexTypeSet -> Maybe (ConvexPolytope Rational) -> ConvexPolytope Rational -> Set VertexTypeSet
goodSetsExcluding _ _ _ Nothing _ = empty
goodSetsExcluding n xs rs (Just angleCP) goodnessCP =
  let (mins, maxs, minEs, maxEs) = minmax n angleCP
  in if last mins == 1 || head maxs == 0 then empty else
      let as = (1 / 2) |*| (last minEs |+| head maxEs)
          xs' = compat n xs as
      in if not . null $ rs `intersection` xs' then empty else
          let goodnessCP' = fromJust $ foldM cutHalfSpace goodnessCP [constraint (asRational v) 0 | v <- elems $ xs' \\ xs]
              vs = elems $ extensionCandidates as mins minEs \\ (xs' `union` rs)
              g = unions $ do
                    (v, prevVs) <- zip vs (inits vs)
                    let angleCP' = cutHyperplane angleCP (HP (asRational v) 2)
                    let goodnessCP'' = fromJust $ cutHalfSpace goodnessCP' (constraint (asRational v) 0)
                    return $ goodSetsExcluding n (insert v xs') (rs `union` fromList prevVs) angleCP' goodnessCP''
          in if xs' /= empty && isGood xs' goodnessCP' then insert xs' g else g

asRational :: Vector Integer -> Vector Rational
asRational = map fromInteger

compatOrthogonalComplementBasis :: Integer -> VertexTypeSet -> [Vector Rational]
compatOrthogonalComplementBasis n xs =
  nullSpaceBasis $ ((genericReplicate n 1) ++ [fromInteger $ n - 2]) : [asRational x ++ [2] | x <- elems xs]

compat :: Integer -> VertexTypeSet -> Vector Rational -> VertexTypeSet
compat n xs as =
  let bs = compatOrthogonalComplementBasis n xs
      cs = sequence [[0..floor (2 / a)] | a <- as]
  in fromList [c | c <- cs, all (\b -> (asRational c ++ [2]) `dot` b == 0) bs]

isGood :: VertexTypeSet -> ConvexPolytope Rational -> Bool
isGood xs goodnessCP =
  let es = extremePoints goodnessCP
  in not $ any (\e -> any (\c -> e `dot` asRational c < 0) xs) es

extensionCandidates :: Vector Rational -> Vector Rational -> [Vector Rational] -> VertexTypeSet
extensionCandidates as mins minEs =
  let as' = if mins ! 2 == 0 then minEs ! 2 else minEs ! 1
      u = as' |-| as
      minsZipUTails = zipPedantic mins (init $ tails u)
      cs = foldl (\vs' (minsI, uTail) -> [vi:v' | v' <- vs', vi <- [0..floor $ boundC v' (minsI, uTail)]]) [[]] (reverse minsZipUTails)
  in fromList [c | c <- cs, asRational c `dot` u >= 0, asRational c `dot` mins <= 2]
  where
    boundC :: Vector Integer -> (Rational, [Rational]) -> Rational
    boundC v' (minsI, uI:u') =
      if minsI > 0
      then 2 / minsI
      else (-1 / uI) * sum [vJ * uJ | (vJ, uJ) <- zipPedantic (asRational v') u']
    boundC _ _ = error "Invalid input."

permutations :: Integer -> VertexTypeSet -> [VertexTypeSet]
permutations n vs = [Set.map (permute p) vs | p <- symmetricGroup n]

rotationsAndReflections :: Integer -> VertexTypeSet -> [VertexTypeSet]
rotationsAndReflections n vs = [Set.map (permute p) vs | p <- dihedralGroup n]

ignoringSymmetries :: Integer -> [VertexTypeSet] -> Set VertexTypeSet -> [VertexTypeSet]
ignoringSymmetries _ [] _ = []
ignoringSymmetries n (vs:vss) except =
  if vs `member` except
    then ignoringSymmetries n vss except
    else vs : (ignoringSymmetries n vss $ union except $ fromList $ rotationsAndReflections n vs)

partitionByDimensionality :: Integer -> [VertexTypeSet] -> [(Integer, [VertexTypeSet])]
partitionByDimensionality n vs =
  toAscList $ foldl (\s v -> Map.insertWith (++) (dimension $ affineSubspace $ snd $ fromJust $ inflate n v) [v] s) Map.empty vs
