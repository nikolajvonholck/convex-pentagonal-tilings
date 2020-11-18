module TilingGraph (TilingGraph, exhaustiveSearch, planarize) where

import Vector (Vector, zero, (|+|), (|-|), (|*|))
import GoodSubsets (VectorType, VectorTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints, fromRationalConvexPolytope)
import AlgebraicNumber (AlgebraicNumber, algebraicNumber, approximate)
import ChebyshevPolynomial (commonDenominator, cosineFieldExtension, sinePoly, cosinePoly)
import Data.List (genericTake, (\\))
import Utils ((!), zipPedantic)
import JSON

import Data.List (genericLength)
import qualified Data.Set as Set
import Data.Set (member, elems)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, insert, delete, fromList)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Control.Monad (guard, foldM)

import Debug.Trace (traceShow)

type Vertex = Integer

data InteriorAngle = AngleA | AngleB | AngleC | AngleD | AngleE deriving (Show, Eq)
data ExteriorAngle = Unknown | Zero | Pi deriving (Show, Eq)
data Angle = Int InteriorAngle | Ext ExteriorAngle deriving (Show, Eq)
data Length = LengthA | LengthB | LengthC | LengthD | LengthE deriving (Show, Eq)

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
    1 -> Int AngleA
    2 -> Int AngleB
    3 -> Int AngleC
    4 -> Int AngleD
    5 -> Int AngleE
    _ -> error "Impossible."

toLength :: Integer -> Length
toLength n =
  case n `oneMod` 5 of
    1 -> LengthA
    2 -> LengthB
    3 -> LengthC
    4 -> LengthD
    5 -> LengthE
    _ -> error "Impossible."

data VertexInfo = VertexInfo Angle Vertex Length deriving (Show, Eq)
angle :: VertexInfo -> Angle
angle (VertexInfo a _ _) = a
vertex :: VertexInfo -> Vertex
vertex (VertexInfo _ v _) = v

lengths :: [Length]
lengths = [LengthA, LengthB, LengthC, LengthD, LengthE]

interiorAngles :: [InteriorAngle]
interiorAngles = [AngleA, AngleB, AngleC, AngleD, AngleE]

-- Map from Vertex to edges with sattelite info: edge comes after angle in counterclockwise rotation around vertex.
-- Maintained invariants:
--   • Each vertex has at most one unknown angle and it must be the first angle.
type TilingGraph = Map Vertex [VertexInfo]

isVertexComplete :: [VertexInfo] -> Bool
isVertexComplete [] = error "Empty vertex info."
isVertexComplete ((VertexInfo (Ext Unknown) _ _):_) = False
isVertexComplete _ = True

vectorType :: [VertexInfo] -> VectorType
vectorType is = [genericLength $ filter ((Int a==) . angle) is | a <- interiorAngles]

vertexInfoList :: TilingGraph -> Vertex -> [VertexInfo]
vertexInfoList g v = case Map.lookup v g of
  Just info -> info
  Nothing -> error "Could not find vertex in graph."

wrappedVertexInfoList :: TilingGraph -> Vertex -> [VertexInfo]
wrappedVertexInfoList g v = let is = vertexInfoList g v in is ++ [head is]

-- after is measured wrt. counterclockwise rotation around v.
vertexInfoAfter :: TilingGraph -> Vertex -> VertexInfo -> VertexInfo
vertexInfoAfter g v i = helper $ wrappedVertexInfoList g v
  where
    helper (x:y:zs) = if x == i then y else helper (y:zs)
    helper _ = error "Could not find vertex info before."

-- Counts the number of each length along the run.
lengthType :: [Length] -> Vector Integer
lengthType ll = [genericLength $ filter (l==) ll | l <- lengths]

isVertexValidHalfVertex :: (VectorTypeSet, VectorTypeSet) -> [VertexInfo] -> Bool
isVertexValidHalfVertex (_, xs) is =
  let vt = vectorType is
  in if isVertexComplete is
    then vt `member` xs
    else any (\x -> all (\(v, w) -> v <= w) $ zip vt x) xs

isVertexValid :: (VectorTypeSet, VectorTypeSet) -> [VertexInfo] -> Bool
isVertexValid xss is =
  let pis = length (filter ((Ext Pi==) . angle) is)
      vt = vectorType is
      xs = case pis of
        0 -> fst xss
        1 -> snd xss
        _ -> error "Impossible: Multiple π angles!"
  in if isVertexComplete is
    then vt `member` xs
    else any (\x -> all (\(v, w) -> v <= w) $ zip vt x) xs

-- Runs counterclockwise in the sense that:
--  - Outermost vertex is clockwise end.
--  - Innermost vertex is counterclockwise end.
data Run = R Vertex ExteriorAngle (Maybe Edge) deriving (Show)
data Edge = Edge Length Run deriving (Show)

-- (counterclockwise beginning, clockwise end)
runEnds :: Run -> (Vertex, Vertex)
runEnds (R v _ e) = case e of
  Nothing -> (error "Impossible: Run of length zero", v)
  Just (Edge _ r) -> (v, snd $ runEnds r)

runBends :: Run -> [Vertex]
runBends (R v a e) = case e of
  Nothing -> []
  Just (Edge _ r) -> if a == Zero then v : runBends r else runBends r

leastVertexAlongRun :: Run -> Vertex
leastVertexAlongRun (R v _ Nothing) = v
leastVertexAlongRun (R v _ (Just (Edge _ r))) = min v $ leastVertexAlongRun r

runLengths :: Run -> [[Length]]
runLengths (R _ a e) = case e of
  Nothing -> [[]]
  Just (Edge s r) ->
    case runLengths r of
      [] -> error "Impossible."
      (p:ps) ->
        case a of
          Zero -> []:(s:p):ps -- Add s finilizing run.
          _ -> (s:p):ps -- Add s to existing run.

exteriorRuns :: TilingGraph -> [Run]
exteriorRuns g =
  let (v, is) = fromJust $ minWhere (not . isVertexComplete . snd) g
      (VertexInfo _ v' s) = head is
      tour = R v Unknown (Just (Edge s (exteriorTour v v v')))
  in partitionTour tour
  where
    -- Returns info about v' and its next length.
    exteriorTour :: Vertex -> Vertex -> Vertex -> Run
    exteriorTour vStart w w' = --(VertexInfo a'' w' _) =
      let i' = fromJust $ find ((w==) . vertex) $ vertexInfoList g w' -- Edge from w' to w.
          (VertexInfo a' w'' s') = vertexInfoAfter g w' i'
      in case a' of
        Int _ -> error "Exterior has interior angle!"
        Ext ea' ->
          if vStart == w' && ea' == Unknown
          then R w' ea' Nothing
          else R w' ea' $ Just (Edge s' (exteriorTour vStart w' w''))

    partitionTour :: Run -> [Run]
    partitionTour (R v Unknown e) = case e of
      Nothing -> []
      Just (Edge s r) -> case r of
        R _ Unknown _ -> partitionTour r
        _ ->
          let (run, r') = collectRun r -- Collect until hitting unknown.
          in (R v Unknown (Just (Edge s run))) : partitionTour r'
    partitionTour _ = error "Exterior run begun unexpectedly."

    collectRun :: Run -> (Run, Run)
    collectRun (R v Unknown e) = (R v Unknown Nothing, R v Unknown e)
    collectRun (R v a e) =
      case e of
        Nothing -> error "Exterior run ended unexpectedly."
        Just (Edge s r) ->
          let (run, r') = collectRun r
          in (R v a (Just (Edge s run)), r')

completeRuns :: (VectorTypeSet, VectorTypeSet) -> (TilingGraph, ConvexPolytope Rational) -> [(TilingGraph, ConvexPolytope Rational)]
completeRuns xss (g, lp) = completeRuns' $ exteriorRuns g
  where
    completeRuns' :: [Run] -> [(TilingGraph, ConvexPolytope Rational)]
    completeRuns' [] = [(g, lp)]
    completeRuns' (r:rs) =
      let (x, y) = runEnds r
      in case map lengthType $ runLengths r of
        [_] -> completeRuns' rs -- No bends to check. Skip this run.
        [la, lb] ->
          let cs = asRational $ la |-| lb -- la < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
            (Just _, Nothing, Nothing) ->
              do
                guard $ isVertexValidHalfVertex xss (vertexInfoList g x)
                completeRuns' rs
            (Nothing, Just _, Nothing) -> mergeVertices xss (g, lp) x y Zero
            (Nothing, Nothing, Just _) ->
              do
                guard $ isVertexValidHalfVertex xss (vertexInfoList g y)
                completeRuns' rs
            (lpleq, lpeq, lpgeq) -> -- Decide run.
              case sequence [lpleq, lpeq, lpgeq] of
                Nothing -> error "Impossible: There should always be an option"
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        [la, lb, lc] ->
          let cs = asRational $ (la |+| lc) |-| lb -- la + lc < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0)) of
            (Just _, Nothing) ->
              do
                guard $ isVertexValidHalfVertex xss (vertexInfoList g x)
                guard $ isVertexValidHalfVertex xss (vertexInfoList g y)
                completeRuns' rs
            (Nothing, Just _) -> mergeVertices xss (g, lp) x y Pi
            (lpleq, lpeq) -> -- Decide run.
              case sequence [lpleq, lpeq] of
                Nothing -> [] -- Backtrack: Violation of triangle inequality.
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        _ -> [] -- Backtrack: Invalid run.

-- v is first in run, v' is last (counterclockwise rotation/run around graph)
-- vertex (min v v') is kept, while (max v v') is discarded.
mergeVertices :: (VectorTypeSet, VectorTypeSet) -> (TilingGraph, ConvexPolytope Rational) -> Vertex -> Vertex -> ExteriorAngle -> [(TilingGraph, ConvexPolytope Rational)]
mergeVertices xss (g, lp) v v' a =
  do
    let (v'', vOut) = (min v v', max v v') -- Choose least vertex index.
    let iss = case (vertexInfoList g v, vertexInfoList g v') of
          ((VertexInfo (Ext Unknown) w s):is, (VertexInfo (Ext Unknown) w' s'):is') ->
            ((VertexInfo (Ext Unknown) w' s'):is') ++ ((VertexInfo (Ext a) w s):is)
          _ -> error "Merging vertices must be incomplete."
    guard $ isVertexValid xss iss -- Backtrack if merged vertex can never be completed.
    let iss' = completeVertex iss
    let g' = Map.map (map (\(VertexInfo a' w s) -> VertexInfo a' (if w == vOut then v'' else w) s)) $ insert v'' iss' $ delete vOut g
    completeRuns xss (g', lp) -- There might be completable runs.
  where
    -- Returns vertex info list, completed if possible.
    completeVertex :: [VertexInfo] -> [VertexInfo]
    completeVertex (iss@((VertexInfo (Ext Unknown) w s):is)) =
      let vt = vectorType is
          a' = if vt `member` snd xss then
                  case length (filter ((Ext Pi==) . angle) iss) of
                    0 -> Pi
                    1 -> Zero
                    _ -> error "Impossible: Multiple π angles!"
                else if vt `member` fst xss then Zero
                else Unknown
      in (VertexInfo (Ext a') w s):is
    completeVertex iss = iss

pickIncompleteVertex :: TilingGraph -> Vertex
pickIncompleteVertex g =
  let weightedIncompleteVertices =
        [(v, v) | v <- Map.keys $ Map.filter (not . isVertexComplete) g] ++
        [(v, weight) | run <- exteriorRuns g,
                       let bends = runBends run,
                       length bends == 2,
                       let weight = leastVertexAlongRun run,
                       let (x, y) = runEnds run,
                       v <- [x, y],
                       not (isVertexComplete (vertexInfoList g v))]
      leastWeight = minimum [w | (_, w) <- weightedIncompleteVertices]
  in minimum [v | (v, w) <- weightedIncompleteVertices, w == leastWeight]

-- Assumes all possible completions performed.
-- Assumptions:
--  • lp is non-empty (this is ensured by type system and ConvexPolytope impl)
--  • The corrected vertex type of every complete vertex lies in xs.
--  • The corrected vertex type of every non-complete vertex "strictly" "respects" xs (i.e is compatible but not immediately completable).
--  • There are no unchecked exteriror (pi/empty) angles.
backtrack :: PolygonConstructor -> Vector Rational -> (VectorTypeSet, VectorTypeSet) -> TilingGraph -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational, Vector Rational)]
backtrack constructor alpha xss g lp =
  do -- Situation: We will have to add another tile.
    let incompleteVertex = pickIncompleteVertex g
    let maxVertexId = fst $ Map.findMax g -- initial 5.
    orientation <- orientations -- Pick direction of new pentagon.
    let anotherTile = pentagonGraph maxVertexId orientation
    let disconnectedGraph = Map.unionWith (\_ _ -> error "Key clash!") g anotherTile
    corner <- interiorAngles
    let cornerVertexId = fst $ fromJust $ minWhere (\(_, is) -> any (\i -> angle i == Int corner) is) anotherTile
    -- Will be glued on in counterclockwise rotation around 'leastIncompleteVertexId'.
    (g', lp') <- mergeVertices xss (disconnectedGraph, lp) cornerVertexId incompleteVertex Zero
    -- All possible completions will be handled inside 'mergeVertices'.
    let construction = constructor lp'
    guard $ isJust construction
    let ls = approximateLengths $ fromJust construction
    (g', lp', ls) : backtrack constructor alpha xss (traceShow ("choice:", incompleteVertex) g') lp'

halfVertexTypes :: VectorTypeSet -> VectorTypeSet
halfVertexTypes xs =
  let withEvenValues = Set.filter (\x -> all (\v -> v `mod` 2 == 0) x) xs
  in Set.map (\x -> [v `div` 2 | v <- x]) withEvenValues

exhaustiveSearch :: VectorTypeSet -> Vector Rational -> ([(TilingGraph, ConvexPolytope Rational, Vector Rational)])
exhaustiveSearch xs alpha =
  let s = angleSum (traceShow alpha alpha)
      (ps, q) = commonDenominator s
      r = cosineFieldExtension q
      sineConstraint = HP [algebraicNumber r (sinePoly p) | p <- ps] 0
      cosineConstraint = HP [algebraicNumber r (cosinePoly p) | p <- ps] 0
      constructor = constructPolygon [sineConstraint, cosineConstraint]
      g = (pentagonGraph 0 CounterClockwise)
      lp = do
        ass <- foldM intersectWithHyperPlane (space 5) [(HP [1, 1, 1, 1, 1] 1)]
        boundedConvexPolytope Strict ass [
            constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
            constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
            constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
            constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
            constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
          ] -- (0, 1)^5
      xss = (xs, halfVertexTypes xs)
  in case traceShow (xs, alpha, s) lp of
    Nothing -> []
    Just lp' -> backtrack constructor alpha xss g lp'

type PolygonConstructor = ConvexPolytope Rational -> Maybe (ConvexPolytope AlgebraicNumber)

constructPolygon :: [HyperPlane AlgebraicNumber] -> PolygonConstructor
constructPolygon cs lp =
  foldM projectOntoHyperplane (fromRationalConvexPolytope lp) cs

-- TODO: Consider defining the two pentagons (in each direction) and using a map to offset vertex ids.
pentagonGraph :: Vertex -> Orientation -> TilingGraph
pentagonGraph vOffset orientation =
  let before n = (n - 1) `oneMod` 5
      after  n = (n + 1) `oneMod` 5
      makeInfo n = case orientation of
        CounterClockwise -> [
            VertexInfo (Ext Unknown) (vOffset + after n) (toLength n),
            VertexInfo (toAngle n) (vOffset + before n) (toLength (before n))
          ]
        ClockWise -> [
            VertexInfo (Ext Unknown) (vOffset + before n) (toLength (before n)),
            VertexInfo (toAngle n) (vOffset + after n) (toLength n)
          ]
  in fromList [(vOffset + i, makeInfo i) | i <- [1..5]]

angleSum :: Vector Rational -> Vector Rational -- TODO: Check tail-part
angleSum as = [fromInteger i - 1 - (sum $ genericTake (i - 1) (tail as)) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)


-- TODO: Consider moving some of it to Javascript UI.
-- The code below is only related to the visualization of the tiling graph.
data VertexWithLocation = VWL Double Double [VertexInfo] deriving (Show, Eq)

approximateLengths :: ConvexPolytope AlgebraicNumber -> Vector Rational
approximateLengths lp =
  let lengthsExtr = elems $ extremePoints lp
  in map (approximate 0.001) $ (1 / (fromInteger $ genericLength lengthsExtr)) |*| (foldl (|+|) (zero 5) lengthsExtr)

planarize :: Vector Rational -> Vector Rational -> TilingGraph -> Map Vertex VertexWithLocation
planarize alpha ls g =
  helper $ Map.fromList [(1, VWL 0 0 (vertexInfoList g 1)), (2, VWL (lengthNum approxLengths LengthA) 0 (vertexInfoList g 2))]
  where
    approxAlpha = map fromRational alpha
    approxLengths = map fromRational ls
    helper :: Map Vertex VertexWithLocation -> Map Vertex VertexWithLocation
    helper gl =
      let vWithL = Map.keys gl
      in case Map.keys g \\ vWithL of
        [] -> gl
        vWithoutL ->
          let v = fromJust $ find (\w -> any (`elem` vWithL) [vertex i | i <- vertexInfoList g w]) vWithoutL
              is = vertexInfoList g v
          in helper $ Map.insert v (determineLocation approxAlpha approxLengths gl v is) gl

determineLocation :: Vector Double -> Vector Double -> Map Vertex VertexWithLocation -> Vertex -> [VertexInfo] -> VertexWithLocation
determineLocation as ls gl v is =
  let VertexInfo _ v' s' = fromJust $ findNeighbourWithLocation gl is
      (VWL xv' yv' isv') = fromJust $ Map.lookup v' gl
      VertexInfo _ v'' _ = fromJust $ findNeighbourWithLocation gl isv'
      (VWL xv'' yv'' _) = fromJust $ Map.lookup v'' gl
      anglev'v'' = (atan2 (yv'' - yv') (xv'' - xv')) / pi
      calcAnglesv' = calcAngles as isv'
      (calcAnglev'', _) = fromJust $ find (\(_, i) -> vertex i == v'') (zipPedantic calcAnglesv' isv')
      (calcAnglev, _) = fromJust $ find (\(_, i) -> vertex i == v) (zipPedantic calcAnglesv' isv')
      anglev = anglev'v'' + calcAnglev - calcAnglev''
      l = lengthNum ls s'
  in VWL (xv' + l * cos (anglev * pi)) (yv' + l * sin (anglev * pi)) is

findNeighbourWithLocation :: Map Vertex VertexWithLocation -> [VertexInfo] -> Maybe VertexInfo
findNeighbourWithLocation gl is =
  let vWithL = Map.keys gl
  in find (\i -> (vertex i) `elem` vWithL) is

angleNum :: Vector Double -> Angle -> Double
angleNum alpha (Int a) = case a of
    AngleA -> alpha ! 1
    AngleB -> alpha ! 2
    AngleC -> alpha ! 3
    AngleD -> alpha ! 4
    AngleE -> alpha ! 5
angleNum _ (Ext a) = case a of
    Unknown -> 0
    Zero -> 0
    Pi -> 1

lengthNum :: Vector Double -> Length -> Double
lengthNum ls s = case s of
    LengthA -> ls ! 1
    LengthB -> ls ! 2
    LengthC -> ls ! 3
    LengthD -> ls ! 4
    LengthE -> ls ! 5

calcAngles :: Vector Double -> [VertexInfo] -> [Double]
calcAngles alpha is = tail $ scanl (\acc i -> acc + angleNum alpha (angle i)) 0 is

instance JSON VertexWithLocation where
  toJSON (VWL x y is) =
    jsonObject [
      ("x", toJSON x),
      ("y", toJSON y),
      ("edges", toJSON is)
    ]

instance JSON VertexInfo where
  toJSON (VertexInfo a v s) =
    jsonObject [
      ("a", toJSON a),
      ("v", show $ toJSON v),
      ("s", show $ toJSON s)
    ]

instance JSON Angle where
  toJSON (Int a) = show $ toJSON a
  toJSON (Ext a) = show $ toJSON a

instance JSON InteriorAngle where
  toJSON AngleA = "1"
  toJSON AngleB = "2"
  toJSON AngleC = "3"
  toJSON AngleD = "4"
  toJSON AngleE = "5"

instance JSON ExteriorAngle where
  toJSON Unknown = "?"
  toJSON Zero = "0"
  toJSON Pi = "pi"

instance JSON Length where
  toJSON LengthA = "1"
  toJSON LengthB = "2"
  toJSON LengthC = "3"
  toJSON LengthD = "4"
  toJSON LengthE = "5"
