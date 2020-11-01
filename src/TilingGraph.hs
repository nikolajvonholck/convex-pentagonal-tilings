module TilingGraph (sides, interiorAngles, isVertexValid, canCompleteVertexWithZero, completeVertexWithZero, canCompleteVertexWithPi, completeVertexWithPi, mergeVertices, exhaustiveSearch) where

import Vector (Vector, (|+|), (|-|), (|*|))
import GoodSubsets (VectorType, VectorTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints)

-- import qualified Data.Set as Set
import Data.List (genericLength)
import qualified Data.Set as Set
import Data.Set (member, elems)
import qualified Data.Map.Strict as Map -- TODO: Consider type of map (strict?, hashmap?, intmap?)
import Data.Map (Map, insert, delete, fromList)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Control.Monad (guard, when)

import Debug.Trace (traceShow)

type Vertex = Integer

data InteriorAngle = AOne | ATwo | AThree | AFour | AFive deriving (Show, Eq)
data ExteriorAngle = Unknown | Zero | Pi deriving (Show, Eq)
data Angle = Int InteriorAngle | Ext ExteriorAngle deriving (Show, Eq)
data Side = EOne | ETwo | EThree | EFour | EFive deriving (Show, Eq)

data Orientation = CounterClockwise | ClockWise

asRational :: Vector Integer -> Vector Rational
asRational = map fromInteger

orientations :: [Orientation]
orientations = [CounterClockwise, ClockWise]

oneMod :: Integer -> Integer -> Integer
n `oneMod` k = ((n - 1) `mod` k) + 1

toAngle :: Integer -> Angle
toAngle n =
  case n `oneMod` 5 of
    1 -> Int AOne
    2 -> Int ATwo
    3 -> Int AThree
    4 -> Int AFour
    5 -> Int AFive
    _ -> error "Impossible."

toSide :: Integer -> Side
toSide n =
  case n `oneMod` 5 of
    1 -> EOne
    2 -> ETwo
    3 -> EThree
    4 -> EFour
    5 -> EFive
    _ -> error "Impossible."

data VertexInfo = VertexInfo Angle Vertex Side deriving (Show, Eq)
angle :: VertexInfo -> Angle
angle (VertexInfo a _ _) = a
vertex :: VertexInfo -> Vertex
vertex (VertexInfo _ v _) = v
-- side :: VertexInfo -> Side
-- side (VertexInfo _ _ s) = s

sides :: [Side]
sides = [EOne, ETwo, EThree, EFour, EFive]

interiorAngles :: [InteriorAngle]
interiorAngles = [AOne, ATwo, AThree, AFour, AFive]

-- nextVertex, map from Vertex to edges with sattelite info: edge comes after angle in counterclockwise rotation around vertex.
-- Maintained invariants:
--   • Each vertex has at most one unknown angle and it must be the first angle.
type TilingGraph = Map Vertex [VertexInfo]

isVertexComplete :: [VertexInfo] -> Bool
isVertexComplete [] = error "Empty vertex info."
isVertexComplete ((VertexInfo (Ext Unknown) _ _):_) = False
isVertexComplete _ = True

vectorType :: [VertexInfo] -> VectorType
vectorType is = [genericLength $ filter ((Int a==) . angle) is | a <- interiorAngles]

correctedVectorType :: [VertexInfo] -> VectorType
correctedVectorType is =
  let c = if isVertexHalf is then 2 else 1
  in c |*| vectorType is

isVertexHalf :: [VertexInfo] -> Bool
isVertexHalf = any ((Ext Pi==) . angle)
-- TODO: Check for multiple PI-angles.

canCompleteVertex :: VectorTypeSet -> [VertexInfo] -> Bool
canCompleteVertex xs is =
  canCompleteVertexWithZero xs is || canCompleteVertexWithPi xs is

canCompleteVertexWithZero :: VectorTypeSet -> [VertexInfo] -> Bool
canCompleteVertexWithZero xs is =
  (not $ isVertexComplete is) && correctedVectorType is `member` xs

canCompleteVertexWithPi :: VectorTypeSet -> [VertexInfo] -> Bool
canCompleteVertexWithPi xs is =
  (not $ isVertexComplete is) && (2 |*| vectorType is `member` xs)

-- Returns final vertex info list along with the adjust vertex info
-- that is used to determine the affected run.
completeVertexWithZero :: [VertexInfo] -> ([VertexInfo], VertexInfo)
completeVertexWithZero [] = error "Empty vertex info."
completeVertexWithZero ((VertexInfo (Ext Unknown) v s):is) =
  let i' = (VertexInfo (Ext Zero) v s) in (i':is, i')
completeVertexWithZero _ = error "First angle of incomplete vertex should be unknown."

completeVertexWithPi :: [VertexInfo] -> ([VertexInfo], VertexInfo)
completeVertexWithPi [] = error "Empty vertex info."
completeVertexWithPi ((VertexInfo (Ext Unknown) v s):is) =
  let i' = (VertexInfo (Ext Pi) v s) in (i':is, i')
completeVertexWithPi _ = error "First angle of incomplete vertex should be unknown."

vertexInfoList :: TilingGraph -> Vertex -> [VertexInfo]
vertexInfoList g v = case Map.lookup v g of
  Just info -> info
  Nothing -> error "Could not find vertex in graph."

wrappedVertexInfoList :: TilingGraph -> Vertex -> [VertexInfo]
wrappedVertexInfoList g v = let is = vertexInfoList g v in is ++ [head is]

-- before is measured wrt. counterclockwise rotation around v.
vertexInfoBefore :: TilingGraph -> Vertex -> VertexInfo -> VertexInfo
vertexInfoBefore g v i = helper $ wrappedVertexInfoList g v
  where
    helper (x:y:zs) = if y == i then x else helper (y:zs)
    helper _ = error "Could not find vertex info before."

-- after is measured wrt. counterclockwise rotation around v.
vertexInfoAfter :: TilingGraph -> Vertex -> VertexInfo -> VertexInfo
vertexInfoAfter g v i = helper $ wrappedVertexInfoList g v
  where
    helper (x:y:zs) = if x == i then y else helper (y:zs)
    helper _ = error "Could not find vertex info before."

runClockwise :: TilingGraph -> Vertex -> VertexInfo -> (Vertex, [VertexInfo])
runClockwise g v i =
  let v' = vertex $ vertexInfoBefore g v i -- next vertex in clockwise run.
      i' = fromJust $ find (\j -> vertex j == v) $ vertexInfoList g v' -- Edge from v' to v.
  in if angle i' == Ext Unknown -- End of run.
    then (v', [i']) -- (begin vertex, first info)
    else let (w, is) = runClockwise g v' i' in (w, i':is)

runCounterClockwise :: TilingGraph -> Vertex -> VertexInfo -> (Vertex, [VertexInfo])
runCounterClockwise g v i =
  let v' = vertex i -- next vertex in counterclockwise run.
      i'' = fromJust $ find (\j -> vertex j == v) $ vertexInfoList g v' -- Edge from v' to v.
      i' = vertexInfoAfter g v' i''
  in if angle i' == Ext Unknown -- End of run.
    then (v', []) -- (begin vertex, first info)
    else let (w, is) = runCounterClockwise g v' i' in (w, i':is)

getRun :: TilingGraph -> Vertex -> VertexInfo -> (Vertex, [VertexInfo], Vertex)
getRun g v i =
  let (x, is') = runClockwise g v i
      (y, is'') = runCounterClockwise g v i
  in (x, (reverse is') ++ [i] ++ is'', y) -- x --> runs ccw to --> y.

getLengths :: [VertexInfo] -> [[Side]]
getLengths [] = [[]]
getLengths ((VertexInfo a _ s):is) =
  case getLengths is of
    (ss:sss) -> case a of
      Ext Zero -> ([]:(s:ss):sss) -- Finish length and initiate next length.
      Ext _ -> (s:ss):sss -- Append to current length.
      Int _ -> error "Should only get lengths for external infos." -- TODO: Improve.
    _ -> error "getLengths Impossible." -- TODO: Improve error.

lengthType :: [Side] -> Vector Integer -- counts lengths.
lengthType ss = [genericLength $ filter (s==) ss | s <- sides]



isVertexValid :: VectorTypeSet -> [VertexInfo] -> Bool
isVertexValid xs is =
  length (filter ((Ext Pi==) . angle) is) <= 1 &&
    let cvt = correctedVectorType is
    in if isVertexComplete is
      then cvt `member` xs
      else any (\x -> all (\(v, w) -> v <= w) $ zip cvt x) xs

vertexCompletions :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational)]
vertexCompletions xs g lp =
  if any (\is -> not $ isVertexValid xs is) g then []
    else case minWhere (\(_, is) -> canCompleteVertex xs is) g of -- Find completable vertex.
      Nothing -> [(g, lp)] -- No more vertices can be completed at this time.
      Just (v, is) ->  -- TODO: Make much more clean...
        let (is', i) = if canCompleteVertexWithZero xs is then completeVertexWithZero is
                  else if canCompleteVertexWithPi xs is then completeVertexWithPi is
                  else error "Some completion should be possible."
            g'' = insert v is' g -- Update vertex info for vertex v. TODO: Consider using Map.adjust.
        in concat [vertexCompletions xs g' lp' | (g', lp') <- completeRun xs g'' lp (v, i)]

-- (v, i) is the affected vertex and angle, so we should consider the induced run.
completeRun :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> (Vertex, VertexInfo) -> [(TilingGraph, ConvexPolytope Rational)]
completeRun xs g lp (v, i) =
  let (x, is, y) = getRun g v i
      checked = case map lengthType $ getLengths is of
        [la, lb] -> let cs = asRational $ la |-| lb -- la < lb
          in [(g, lp') | let lp'' = cutHalfSpace lp (constraint cs 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g, lp') | let lp'' = cutHalfSpace lp (constraint ((-1) |*| cs) 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g', lp') | let lp'' = projectOntoHyperplane lp (HP cs 0),
                            isJust lp'',
                            let (Just lp''') = lp'',
                            (g', lp') <- mergeVertices xs g lp''' x y Zero]
        [la, lb, lc] -> let cs = asRational $ (la |+| lc) |-| lb -- la + lc < lb
          in [(g, lp') | let lp'' = cutHalfSpace lp (constraint cs 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g', lp') | let lp'' = projectOntoHyperplane lp (HP cs 0),
                            isJust lp'',
                            let (Just lp''') = lp'',
                            (g', lp') <- mergeVertices xs g lp''' x y Pi]
        [_] -> [(g, lp)]
        _ -> error $ "Not zero, one or two Zero-angles." ++ show is
  in do
    (g', lp') <- checked
    vertexCompletions xs g' lp'

-- v is first in run, v' is last (counterclockwise rotation/run around graph)
-- vertex (min v v') is kept, while (max v v') is discarded.
mergeVertices :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> Vertex -> Vertex -> ExteriorAngle -> [(TilingGraph, ConvexPolytope Rational)]
mergeVertices xs g lp v v' a =
  let (v'', vOut) = (min v v', max v v') -- Choose least vertex index.
      (iss, i') = case (vertexInfoList g v, vertexInfoList g v') of
        ((VertexInfo (Ext Unknown) w s):is, (VertexInfo (Ext Unknown) w' s'):is') ->
          let i'' = VertexInfo (Ext a) w s -- TODO: Can simplify lines around this point.
          in (((VertexInfo (Ext Unknown) w' s'):is') ++ (i'':is), i'')
        _ -> error "Merging vertices must be incomplete."
      g' = insert v'' iss $ delete vOut g
      -- TODO: Assert that i' does not mention v or v'. (it should not be 'outdated')
      g'' = Map.map (map (\(VertexInfo a' w s) -> VertexInfo a' (if w == vOut then v'' else w) s)) g'
  in do
    let info = vertexInfoList g'' v''
    when (i' `notElem` info) (error "i' was changed during merge.")
    guard $ isVertexValid xs info -- Abandon early if merged vertex can never be completed.
    completeRun xs g'' lp (v'', i') -- Note if v'' can be completed, it will happen inside of here.

-- Assumes all possible completions performed.
-- Assumptions:
--  • lp is non-empty (this is ensured by type system and ConvexPolytope impl)
--  • The corrected vertex type of every complete vertex lies in xs.
--  • The corrected vertex type of every non-complete vertex "strictly" "respects" xs (i.e is compatible but not immediately completable).
--  • There are no unchecked exteriror (pi/empty) angles.
backtrack :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational)]
backtrack xs g lp =
  -- Conclusion: We will have to add another tile.
  do
    let maxVertexId = fst $ Map.findMax g -- initial 5.
    let leastIncompleteVertexId = fst $ fromJust $ minWhere (\(_, is) -> not $ isVertexComplete is) (traceShow g g) -- initial 1.
    orientation <- orientations -- Pick direction of new pentagon.
    let anotherTile = pentagonGraph (traceShow maxVertexId maxVertexId) orientation
    let disconnectedGraph = Map.unionWith (\_ _ -> error "Key clash!") g anotherTile
    corner <- interiorAngles
    let cornerVertexId = fst $ fromJust $ minWhere (\(_, is) -> any (\i -> angle i == Int corner) is) anotherTile
    (g', lp') <- mergeVertices xs disconnectedGraph lp cornerVertexId leastIncompleteVertexId Zero -- Will be glued on in counterclockwise rotation around 'leastIncompleteVertexId'.
    -- all possible completions will be handled inside 'mergeVertices'.

    -- TODO: Consider guard isConstructible g' lp'
    backtrack xs g' lp'

exhaustiveSearch :: VectorTypeSet -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational)]
exhaustiveSearch xs angleCP =
  let points = extremePoints angleCP
  in case elems points of
    [_] ->
      let -- (ps, q) = commonDenominator x
          -- r = cosineFieldExtension q
          -- sineConstraint = HP [algebraicNumber r (sinePoly p) | p <- ps] 0
          -- cosineConstraint = HP [algebraicNumber r (cosinePoly p) | p <- ps] 0
          ass = fromJust $ intersectWithHyperPlane (space 5) (HP [1, 1, 1, 1, 1] 1)
          cp = fromJust $ boundedConvexPolytope Strict ass $ Set.fromList [
              constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
              constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
              constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
              constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
              constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
            ] -- (0, 1)^5
          -- sideCP <- foldM projectOntoHyperplane cp [sineConstraint, cosineConstraint]
          g = (pentagonGraph 0 CounterClockwise)
      in backtrack (traceShow xs xs) g cp
    _ -> []

-- TODO: Consider defining the two pentagons (in each direction) and using a map to offset vertex ids.
pentagonGraph :: Vertex -> Orientation -> TilingGraph
pentagonGraph vOffset orientation =
  let before n = (n - 1) `oneMod` 5
      after  n = (n + 1) `oneMod` 5
      makeInfo n = case orientation of
        CounterClockwise -> [
            VertexInfo (Ext Unknown) (vOffset + after n) (toSide n),
            VertexInfo (toAngle n) (vOffset + before n) (toSide (before n))
          ]
        ClockWise -> [
            VertexInfo (Ext Unknown) (vOffset + before n) (toSide (before n)),
            VertexInfo (toAngle n) (vOffset + after n) (toSide n)
          ]
  in fromList [(vOffset + i, makeInfo i) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)

-- maxWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
-- maxWhere p m = find p (Map.toDescList m)
