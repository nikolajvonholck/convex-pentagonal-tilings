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

-- import qualified Data.Set as Set
import Data.List (genericLength)
import Data.Set (member, elems)
import qualified Data.Map.Strict as Map -- TODO: Consider type of map (strict?, hashmap?, intmap?)
import Data.Map (Map, insert, delete, fromList)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Control.Monad (guard, when, foldM)

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

-- Returns final vertex info list along with the adjusted vertex info
-- that can be used to determine the affected run.
completeVertex :: VectorTypeSet -> [VertexInfo] -> Maybe ([VertexInfo], VertexInfo)
completeVertex xs iss =
  case iss of
    ((VertexInfo (Ext Unknown) v s):is) -> do
        a <- if correctedVectorType is `member` xs then Just Zero
             else if 2 |*| vectorType is `member` xs then Just Pi
             else Nothing
        let i' = (VertexInfo (Ext a) v s)
        return (i':is, i')
    _ -> Nothing

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

-- Runs counterclockwise in the sense that:
--  - Outermost vertex is clockwise end.
--  - Innermost vertex is counterclockwise end.
data Run = R Vertex ExteriorAngle (Maybe Edge) deriving (Show)
data Edge = Edge Side Run deriving (Show)

-- (counterclockwise beginning, clockwise end)
runEnds :: Run -> (Vertex, Vertex)
runEnds (R v _ e) = case e of
  Nothing -> (error "Impossible", v)
  Just (Edge _ r) -> (v, snd $ runEnds r)

runBends :: Run -> [Vertex]
runBends (R v a e) = case e of
  Nothing -> []
  Just (Edge _ r) -> if a == Zero then v : runBends r else runBends r

leastVertexAlongRun :: Run -> Vertex
leastVertexAlongRun (R v _ Nothing) = v
leastVertexAlongRun (R v _ (Just (Edge _ r))) = min v $ leastVertexAlongRun r

runSides :: Run -> [[Side]]
runSides (R _ a e) = case e of
  Nothing -> [[]]
  Just (Edge s r) ->
    case runSides r of
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
    -- Returns info about v' and its next side.
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

isRunValid :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> Run -> Bool
isRunValid xs g lp r =
  let (x, y) = runEnds r
  in case map lengthType $ runSides r of
    [la, lb] ->
      let cs = asRational $ la |-| lb -- la < lb
      in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
        (Just _, Nothing, Nothing) -> isVertexValidHalfVertex xs $ vertexInfoList g x
        (Nothing, Just _, Nothing) -> error "Impossible: Vertices should already have been merged."
        (Nothing, Nothing, Just _) -> isVertexValidHalfVertex xs $ vertexInfoList g y
        _ -> error "Impossible: Run has not been decided."
    [la, lb, lc] ->
      let cs = asRational $ (la |+| lc) |-| lb -- la + lc < lb
      in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
        (Just _, Nothing, Nothing) -> isVertexValidHalfVertex xs (vertexInfoList g x) && isVertexValidHalfVertex xs (vertexInfoList g y)
        (Nothing, Just _, Nothing) -> error "Impossible: Vertices should already have been merged."
        (Nothing, Nothing, Just _) -> error "Impossible: Violation of triangle inequality."
        _ -> error "Impossible: Run has not been decided."
    [_] -> True
    _ -> error "Impossible: Malformed run."

getRun :: TilingGraph -> Vertex -> VertexInfo -> Run
getRun g v (VertexInfo (Ext a) v' s) =
  let runs = exteriorRuns g
  in fromJust $ find runHasEdge runs
  where
    runHasEdge :: Run -> Bool
    runHasEdge (R _ _ Nothing) = False
    runHasEdge (R w a' (Just (Edge s' r'))) =
      let (R w' _ _) = r'
      in (v, v', a, s) == (w, w', a', s') || runHasEdge r'
getRun _ _ (VertexInfo (Int _) _ _) = error "Can not find run with interior angle."

-- Counts the number of each length along the run.
lengthType :: [Side] -> Vector Integer
lengthType ss = [genericLength $ filter (s==) ss | s <- sides]

isVertexValidHalfVertex :: VectorTypeSet -> [VertexInfo] -> Bool
isVertexValidHalfVertex xs is =
  let numPIs = length (filter ((Ext Pi==) . angle) is)
      cvt = 2 |*| vectorType is
  in if isVertexComplete is
    then numPIs == 1 && cvt `member` xs
    else numPIs == 0 && (any (\x -> all (\(v, w) -> v <= w) $ zip cvt x) xs)

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
    else case Map.lookupMin $ Map.filter isJust $ Map.map (completeVertex xs) g of -- Find completable vertex.
      Nothing -> [(g, lp)] -- No more vertices can be completed at this time.
      Just (v, Just (is', i)) ->
        let g'' = insert v is' g -- Update vertex info for vertex v. TODO: Consider using Map.adjust.
        in concat [vertexCompletions xs g' lp' | (g', lp') <- completeRun xs g'' lp (v, i)]
      _ -> error "Impossible."

-- (v, i) is the affected vertex and angle, so we should consider the induced run.
completeRun :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> (Vertex, VertexInfo) -> [(TilingGraph, ConvexPolytope Rational)]
completeRun xs g lp (v, i) =
  let r = getRun g v i
      (x, y) = runEnds r
      checked = case map lengthType $ runSides r of
        [la, lb] -> let cs = asRational $ la |-| lb -- la < lb
          in [(g, lp') | let lp'' = cutHalfSpace lp (constraint cs 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g, lp') | let lp'' = cutHalfSpace lp (constraint ((-1) |*| cs) 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g', lp') | let lp'' = projectOntoHyperplane lp (HP cs 0),
                            isJust lp'',
                            let (Just lp') = lp'',
                            let (g', v', _) = mergeTwoVertices g x y Zero,
                            isVertexValid xs $ vertexInfoList g' v']
        [la, lb, lc] -> let cs = asRational $ (la |+| lc) |-| lb -- la + lc < lb
          in [(g, lp') | let lp'' = cutHalfSpace lp (constraint cs 0), isJust lp'', let (Just lp') = lp'']
            ++ [(g', lp') | let lp'' = projectOntoHyperplane lp (HP cs 0),
                            isJust lp'',
                            let (Just lp') = lp'',
                            let (g', v', _) = mergeTwoVertices g x y Pi,
                            isVertexValid xs $ vertexInfoList g' v']
        [_] -> [(g, lp)]
        _ -> []
  in do
    (g', lp') <- checked
    vertexCompletions xs g' lp'

-- v is first in run, v' is last (counterclockwise rotation/run around graph)
-- vertex (min v v') is kept, while (max v v') is discarded.
mergeTwoVertices :: TilingGraph -> Vertex -> Vertex -> ExteriorAngle -> (TilingGraph, Vertex, VertexInfo)
mergeTwoVertices g v v' a =
  let (v'', vOut) = (min v v', max v v') -- Choose least vertex index.
      (iss, i') = case (vertexInfoList g v, vertexInfoList g v') of
        ((VertexInfo (Ext Unknown) w s):is, (VertexInfo (Ext Unknown) w' s'):is') ->
          let i'' = VertexInfo (Ext a) w s
          in (((VertexInfo (Ext Unknown) w' s'):is') ++ (i'':is), i'')
        _ -> error "Merging vertices must be incomplete."
      g' = insert v'' iss $ delete vOut g
      -- TODO: Consider assert that i' does not mention v or v'. (it should not be 'outdated')
      g'' = Map.map (map (\(VertexInfo a' w s) -> VertexInfo a' (if w == vOut then v'' else w) s)) g'
  in (g'', v'', i')

mergeVertices :: VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> Vertex -> Vertex -> ExteriorAngle -> [(TilingGraph, ConvexPolytope Rational)]
mergeVertices xs g lp v v' a =
  do
    let (g'', v'', i') = mergeTwoVertices g v v' a
    let info = vertexInfoList g'' v''
    when (i' `notElem` info) (error "i' was changed during merge.")
    guard $ isVertexValid xs info -- Abandon early if merged vertex can never be completed.
    -- TODO: Maybe check half vertices??
    completeRun xs g'' lp (v'', i') -- Note if v'' can be completed, it will happen inside of here.

-- Assumes all possible completions performed.
-- Assumptions:
--  • lp is non-empty (this is ensured by type system and ConvexPolytope impl)
--  • The corrected vertex type of every complete vertex lies in xs.
--  • The corrected vertex type of every non-complete vertex "strictly" "respects" xs (i.e is compatible but not immediately completable).
--  • There are no unchecked exteriror (pi/empty) angles.
backtrack :: PolygonConstructor -> Vector Rational -> VectorTypeSet -> TilingGraph -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational, Vector Rational)]
backtrack constructor alpha xs g lp =
  -- Conclusion: We will have to add another tile.
  do
    let maxVertexId = fst $ Map.findMax g -- initial 5.
    let defaultWeights = [(v, v) | v <- Map.keys $ Map.filter (not . isVertexComplete) g]
    let twoBendWeights = [(v, weight) |
          run <- exteriorRuns g,
          let bends = runBends run,
          length bends == 2,
          let weight = leastVertexAlongRun run,
          let (x, y) = runEnds run,
          v <- [x, y],
          not (isVertexComplete (vertexInfoList g v))]
    let weights = defaultWeights ++ twoBendWeights
    let leastWeight = minimum [w | (_, w) <- weights]
    let incompleteVertex = minimum [v | (v, w) <- weights, w == leastWeight]
    orientation <- orientations -- Pick direction of new pentagon.
    let anotherTile = pentagonGraph maxVertexId orientation
    let disconnectedGraph = Map.unionWith (\_ _ -> error "Key clash!") g anotherTile
    corner <- interiorAngles
    let cornerVertexId = fst $ fromJust $ minWhere (\(_, is) -> any (\i -> angle i == Int corner) is) anotherTile
    (g', lp') <- mergeVertices xs disconnectedGraph lp cornerVertexId incompleteVertex Zero -- Will be glued on in counterclockwise rotation around 'leastIncompleteVertexId'.
    guard $ all (isRunValid xs g' lp') (exteriorRuns g')
    -- all possible completions will be handled inside 'mergeVertices'.
    let construction = constructor lp'
    guard $ isJust construction
    let lengths = approximateLengths $ fromJust construction
    (g', lp', lengths) : backtrack constructor alpha xs (traceShow ("choice:", incompleteVertex) g') lp'

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
  in case traceShow (xs, alpha, s) lp of
    Nothing -> []
    Just lp' -> backtrack constructor alpha xs g lp'

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
            VertexInfo (Ext Unknown) (vOffset + after n) (toSide n),
            VertexInfo (toAngle n) (vOffset + before n) (toSide (before n))
          ]
        ClockWise -> [
            VertexInfo (Ext Unknown) (vOffset + before n) (toSide (before n)),
            VertexInfo (toAngle n) (vOffset + after n) (toSide n)
          ]
  in fromList [(vOffset + i, makeInfo i) | i <- [1..5]]

angleSum :: Vector Rational -> Vector Rational -- TODO: Check tail-part
angleSum as = [fromInteger i - 1 - (sum $ genericTake (i - 1) (tail as)) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)

-- The code below is only related to the visualization of the tiling graph.
data VertexWithLocation = VWL Double Double [VertexInfo] deriving (Show, Eq)

approximateLengths :: ConvexPolytope AlgebraicNumber -> Vector Rational
approximateLengths lp =
  let lengthsExtr = elems $ extremePoints lp
  in map (approximate 0.001) $ (1 / (fromInteger $ genericLength lengthsExtr)) |*| (foldl (|+|) (zero 5) lengthsExtr)

planarize :: Vector Rational -> Vector Rational -> TilingGraph -> Map Vertex VertexWithLocation
planarize alpha lengths g =
  helper $ Map.fromList [(1, VWL 0 0 (vertexInfoList g 1)), (2, VWL (lengthNum approxLengths EOne) 0 (vertexInfoList g 2))]
  where
    approxAlpha = map fromRational alpha
    approxLengths = map fromRational lengths
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
    AOne -> alpha ! 1
    ATwo -> alpha ! 2
    AThree -> alpha ! 3
    AFour -> alpha ! 4
    AFive -> alpha ! 5
angleNum _ (Ext a) = case a of
    Unknown -> 0
    Zero -> 0
    Pi -> 1

lengthNum :: Vector Double -> Side -> Double
lengthNum lengths s = case s of
    EOne -> lengths ! 1
    ETwo -> lengths ! 2
    EThree -> lengths ! 3
    EFour -> lengths ! 4
    EFive -> lengths ! 5

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
  toJSON AOne = "1"
  toJSON ATwo = "2"
  toJSON AThree = "3"
  toJSON AFour = "4"
  toJSON AFive = "5"

instance JSON ExteriorAngle where
  toJSON Unknown = "?"
  toJSON Zero = "0"
  toJSON Pi = "pi"

instance JSON Side where
  toJSON EOne = "1"
  toJSON ETwo = "2"
  toJSON EThree = "3"
  toJSON EFour = "4"
  toJSON EFive = "5"
