module TilingGraph (TilingGraph, exhaustiveSearch) where

import Vector (Vector, zero, (|+|), (|-|), (|*|))
import GoodSubsets (VectorType, VectorTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints, fromRationalConvexPolytope)
import AlgebraicNumber (AlgebraicNumber, algebraicNumber, approximate)
import ChebyshevPolynomial (commonDenominator, cosineFieldExtension, sinePoly, cosinePoly)
import Data.List (genericTake)
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
data Length = LengthA | LengthB | LengthC | LengthD | LengthE deriving (Show, Eq)
data Orientation = CounterClockwise | ClockWise

asRational :: Vector Integer -> Vector Rational
asRational = map fromInteger

orientations :: [Orientation]
orientations = [CounterClockwise, ClockWise]

oneMod :: Integer -> Integer -> Integer
n `oneMod` k = ((n - 1) `mod` k) + 1

toAngle :: Integer -> InteriorAngle
toAngle n =
  case n `oneMod` 5 of
    1 -> AngleA
    2 -> AngleB
    3 -> AngleC
    4 -> AngleD
    5 -> AngleE
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

-- TODO: Documentation.
data Corner = Corner ExteriorAngle (Length, Vertex) InteriorAngle (Length, Vertex) deriving (Show, Eq)
interiorAngle :: Corner -> InteriorAngle
interiorAngle (Corner _ _ ia _) = ia
exteriorAngle :: Corner -> ExteriorAngle
exteriorAngle (Corner ea _ _ _) = ea

lengths :: [Length]
lengths = [LengthA, LengthB, LengthC, LengthD, LengthE]

interiorAngles :: [InteriorAngle]
interiorAngles = [AngleA, AngleB, AngleC, AngleD, AngleE]

-- Map from Vertex to edges with sattelite info: edge comes after angle in counterclockwise rotation around vertex.
-- Maintained invariants:
--   • Each vertex has at most one unknown angle and it must be the first angle.
type TilingGraph = Map Vertex [Corner]

isVertexComplete :: [Corner] -> Bool
isVertexComplete ((Corner Unknown _ _ _):_) = False
isVertexComplete _ = True

vectorType :: [Corner] -> VectorType
vectorType cs = [genericLength $ filter (\c -> interiorAngle c == a) cs | a <- interiorAngles]

cornerList :: TilingGraph -> Vertex -> [Corner]
cornerList g v = case Map.lookup v g of
  Just cs -> cs
  Nothing -> error "Could not find vertex in graph."

wrappedCornerList :: TilingGraph -> Vertex -> [Corner]
wrappedCornerList g v = let cs = cornerList g v in cs ++ [head cs]

-- after is measured wrt. counterclockwise rotation around v.
cornerAfter :: TilingGraph -> Vertex -> Corner -> Corner
cornerAfter g v i = helper $ wrappedCornerList g v
  where
    helper (x:y:zs) = if x == i then y else helper (y:zs)
    helper _ = error "Could not find corner after given corner."

-- Counts the number of each length along the given run.
lengthCounts :: [Length] -> Vector Integer
lengthCounts ll = [genericLength $ filter (l==) ll | l <- lengths]

isVertexValidHalfVertex :: (VectorTypeSet, VectorTypeSet) -> [Corner] -> Bool
isVertexValidHalfVertex (_, xs) cs =
  let vt = vectorType cs
  in if isVertexComplete cs
    then vt `member` xs
    else any (\x -> all (\(v, w) -> v <= w) $ zip vt x) xs

isVertexValid :: (VectorTypeSet, VectorTypeSet) -> [Corner] -> Bool
isVertexValid xss cs =
  let pis = length (filter (\c -> exteriorAngle c == Pi) cs)
      vt = vectorType cs
      xs = case pis of
        0 -> fst xss
        1 -> snd xss
        _ -> error "Impossible: Multiple π angles!"
  in if isVertexComplete cs
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
  let (v, cs) = fromJust $ minWhere (not . isVertexComplete . snd) g
      (Corner _ (l, v') _ _) = head cs
      tour = R v Unknown (Just (Edge l (exteriorTour v v v')))
  in partitionTour tour
  where
    -- Returns info about v' and its next length.
    exteriorTour :: Vertex -> Vertex -> Vertex -> Run
    exteriorTour vStart w w' =
      let c' = fromJust $ find (\(Corner _ _ _ (_, z)) -> z == w) $ cornerList g w' -- Edge from w' to w.
          (Corner ea' (s', w'') _ _) = cornerAfter g w' c'
      in if vStart == w' && ea' == Unknown
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
      in case map lengthCounts $ runLengths r of
        [_] -> completeRuns' rs -- No bends to check. Skip this run.
        [la, lb] ->
          let cs = asRational $ la |-| lb -- la < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
            (Just _, Nothing, Nothing) ->
              do
                guard $ isVertexValidHalfVertex xss (cornerList g x)
                completeRuns' rs
            (Nothing, Just _, Nothing) -> mergeVertices xss (g, lp) x y Zero
            (Nothing, Nothing, Just _) ->
              do
                guard $ isVertexValidHalfVertex xss (cornerList g y)
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
                guard $ isVertexValidHalfVertex xss (cornerList g x)
                guard $ isVertexValidHalfVertex xss (cornerList g y)
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
    let iss = case (cornerList g v, cornerList g v') of
          ((Corner Unknown e1 ia e2):cs, (Corner Unknown e1' ia' e2'):cs') ->
            ((Corner Unknown e1' ia' e2'):cs') ++ ((Corner a e1 ia e2):cs)
          _ -> error "Merging vertices must be incomplete."
    guard $ isVertexValid xss iss -- Backtrack if merged vertex can never be completed.
    let (vKeep, vDiscard) = (min v v', max v v') -- Choose to keep least vertex index.
    let iss' = completeVertex iss
    let g' = Map.map (map $ renameVertex vDiscard vKeep) $ insert vKeep iss' $ delete vDiscard g
    completeRuns xss (g', lp) -- There might be completable runs.
  where-- w == vDiscard, w' == vKeep
    renameVertex :: Vertex -> Vertex -> Corner -> Corner
    renameVertex w w' (Corner ea (s1, z1) ia (s2, z2)) =
      let swap z = if z == w then w' else z
      in Corner ea (s1, swap z1) ia (s2, swap z2)
    -- Returns vertex info list, completed if possible.
    completeVertex :: [Corner] -> [Corner]
    completeVertex (css@((Corner Unknown e1 ia e2):is)) =
      let vt = vectorType css
          ea = if vt `member` snd xss then
                  case length (filter (\c -> exteriorAngle c == Pi) css) of
                    0 -> Pi
                    1 -> Zero
                    _ -> error "Impossible: Multiple π angles!"
                else if vt `member` fst xss then Zero
                else Unknown
      in (Corner ea e1 ia e2):is
    completeVertex css = css

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
                       not (isVertexComplete (cornerList g v))]
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
    let cornerVertexId = fst $ fromJust $ minWhere (\(_, cs) -> any (\c -> interiorAngle c == corner) cs) anotherTile
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
      makeCorners n = case orientation of
        CounterClockwise ->
          [Corner Unknown (toLength n, vOffset + after n) (toAngle n) (toLength (before n), vOffset + before n)]
        ClockWise ->
          [Corner Unknown (toLength (before n), vOffset + before n) (toAngle n) (toLength n, vOffset + after n)]
  in fromList [(vOffset + i, makeCorners i) | i <- [1..5]]

angleSum :: Vector Rational -> Vector Rational -- TODO: Check tail-part
angleSum as = [fromInteger i - 1 - (sum $ genericTake (i - 1) (tail as)) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)

approximateLengths :: ConvexPolytope AlgebraicNumber -> Vector Rational
approximateLengths lp =
  let lengthsExtr = elems $ extremePoints lp
  in map (approximate 0.001) $ (1 / (fromInteger $ genericLength lengthsExtr)) |*| (foldl (|+|) (zero 5) lengthsExtr)

-- TODO: Must be part of array as of now...
instance JSON Corner where
  toJSON (Corner ea (l1, v1) ia (l2, v2)) =
    jsonObject [
        ("a", toJSON ea),
        ("l", toJSON l1),
        ("v", show $ toJSON v1)
      ]
    ++ ", " ++
    jsonObject [
        ("a", toJSON ia),
        ("l", toJSON l2),
        ("v", show $ toJSON v2)
      ]

instance JSON InteriorAngle where
  toJSON AngleA = show "A"
  toJSON AngleB = show "B"
  toJSON AngleC = show "C"
  toJSON AngleD = show "D"
  toJSON AngleE = show "E"

instance JSON ExteriorAngle where
  toJSON Unknown = show "?"
  toJSON Zero = show "0"
  toJSON Pi = show "Pi"

instance JSON Length where
  toJSON LengthA = show "a"
  toJSON LengthB = show "b"
  toJSON LengthC = show "c"
  toJSON LengthD = show "d"
  toJSON LengthE = show "e"
