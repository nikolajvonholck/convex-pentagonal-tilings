module TilingGraph (TilingGraph, exhaustiveSearch) where

import Vector (Vector, zero, (|+|), (|-|), (|*|))
import GoodSet (VertexType, VertexTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space, subset)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints, fromRationalConvexPolytope, affineSubspace)
import AlgebraicNumber (AlgebraicNumber, algebraicNumber, approximate)
import ChebyshevPolynomial (commonDenominator, cosineFieldExtension, sinePoly, cosinePoly)
import Type (Type(..), knownTypes)
import Utils ((!), minBy)
import JSON

import qualified Data.Set as Set
import Data.Set (Set, empty, member, notMember, elems)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, insert, delete, fromList)
import Data.Maybe (fromJust)
import Data.List (find, genericTake, genericLength, sortOn)
import Control.Monad (guard, foldM)
import qualified Data.Tuple as Tuple

import Debug.Trace (traceShow)

type Vertex = Integer

data InteriorAngle = AngleA | AngleB | AngleC | AngleD | AngleE deriving (Show, Eq)
data ExteriorAngle = Unknown | Zero | Pi deriving (Show, Eq)
data Length = LengthA | LengthB | LengthC | LengthD | LengthE deriving (Show, Eq)
data Orientation = CounterClockwise | ClockWise deriving (Show, Eq, Ord)

asAlgNum :: Vector Integer -> Vector AlgebraicNumber
asAlgNum = map fromInteger

orientations :: [Orientation]
orientations = [CounterClockwise, ClockWise]

-- A corner of a pentagon is represented by giving the exterior angle, then the
-- length to the first vertex, then the interior angle and lastly the length to
-- the second vertex, all listed in counterclockwise rotation around the corner.
data Corner = Corner ExteriorAngle (Length, Vertex) InteriorAngle (Length, Vertex) deriving (Show, Eq)
interiorAngle :: Corner -> InteriorAngle
interiorAngle (Corner _ _ ia _) = ia
exteriorAngle :: Corner -> ExteriorAngle
exteriorAngle (Corner ea _ _ _) = ea

lengths :: [Length]
lengths = [LengthA, LengthB, LengthC, LengthD, LengthE]

interiorAngles :: [InteriorAngle]
interiorAngles = [AngleA, AngleB, AngleC, AngleD, AngleE]

-- Map from Vertex to list of pentagon corners listed in counterclockwise
-- rotation around the vertex. We maintain the invariant that a vertex has at
-- most one unknown exterior angle and it must be the first angle.
type TilingGraph = Map Vertex [Corner]

isVertexComplete :: [Corner] -> Bool
isVertexComplete ((Corner Unknown _ _ _):_) = False
isVertexComplete _ = True

vertexType :: [Corner] -> VertexType
vertexType cs = [genericLength $ filter (\c -> interiorAngle c == a) cs | a <- interiorAngles]

cornerList :: TilingGraph -> Vertex -> [Corner]
cornerList g v = fromJust $ Map.lookup v g

wrappedCornerList :: TilingGraph -> Vertex -> [Corner]
wrappedCornerList g v = let cs = cornerList g v in cs ++ [head cs]

-- The corner after the given corner is found by counterclockwise rotation
-- around the given vertex.
cornerAfter :: TilingGraph -> Vertex -> Corner -> Corner
cornerAfter g v i = helper $ wrappedCornerList g v
  where
    helper (x:y:zs) = if x == i then y else helper (y:zs)
    helper _ = error "Could not find corner after given corner."

-- Counts the number of each length.
lengthCounts :: [Length] -> Vector Integer
lengthCounts ll = [genericLength $ filter (l==) ll | l <- lengths]

isVertexValidHalfVertex :: (VertexTypeSet, VertexTypeSet) -> [Corner] -> Bool
isVertexValidHalfVertex (_, xs) cs =
  let vt = vertexType cs
  in if isVertexComplete cs
    then vt `member` xs
    else 0 < Set.size (compatibleVertexTypes xs vt)

isVertexValid :: (VertexTypeSet, VertexTypeSet) -> [Corner] -> Bool
isVertexValid xss cs =
  let pis = length (filter (\c -> exteriorAngle c == Pi) cs)
      vt = vertexType cs
      xs = case pis of
        0 -> fst xss
        1 -> snd xss
        _ -> error "Impossible: Multiple π angles!"
  in if isVertexComplete cs
    then vt `member` xs
    else 0 < Set.size (compatibleVertexTypes xs vt)

compatibleVertexTypes :: VertexTypeSet -> VertexType -> VertexTypeSet
compatibleVertexTypes xs vt =
  Set.filter (\x -> all (\(v, w) -> v <= w) $ zip vt x) xs

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

type HalfInDirection = (Vertex, Orientation)

completeRuns :: (VertexTypeSet, VertexTypeSet) -> (TilingGraph, ConvexPolytope AlgebraicNumber) -> [(TilingGraph, Set HalfInDirection, ConvexPolytope AlgebraicNumber)]
completeRuns xss (g, lp) = completeRuns' empty $ exteriorRuns g
  where
    completeRuns' :: Set HalfInDirection -> [Run] -> [(TilingGraph, Set HalfInDirection, ConvexPolytope AlgebraicNumber)]
    completeRuns' hv [] = [(g, hv, lp)]
    completeRuns' hvs (r:rs) =
      let (x, y) = runEnds r
      in case map lengthCounts $ runLengths r of
        [_] -> completeRuns' hvs rs -- No bends to check. Skip this run.
        [la, lb] ->
          let cs = asAlgNum $ la |-| lb -- la < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
            (Just _, Nothing, Nothing) ->
              do
                guard $ isVertexValidHalfVertex xss (cornerList g x)
                let hv = (x, ClockWise)
                guard $ (x, CounterClockwise) `notMember` hvs
                completeRuns' (Set.insert hv hvs) rs
            (Nothing, Just _, Nothing) -> mergeVertices xss (g, lp) x y Zero
            (Nothing, Nothing, Just _) ->
              do
                guard $ isVertexValidHalfVertex xss (cornerList g y)
                let hv = (y, CounterClockwise)
                guard $ (y, ClockWise) `notMember` hvs
                completeRuns' (Set.insert hv hvs) rs
            (lpleq, lpeq, lpgeq) -> -- Decide run.
              case sequence [lpleq, lpeq, lpgeq] of
                Nothing -> error "Impossible: There should always be an option"
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        [la, lb, lc] ->
          let cs = asAlgNum $ (la |+| lc) |-| lb -- la + lc < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0)) of
            (Just _, Nothing) ->
              do
                guard $ isVertexValidHalfVertex xss (cornerList g x)
                guard $ isVertexValidHalfVertex xss (cornerList g y)
                let hvx = (x, ClockWise)
                let hvy = (y, CounterClockwise)
                guard $ (x, CounterClockwise) `notMember` hvs
                guard $ (y, ClockWise) `notMember` hvs
                completeRuns' (Set.insert hvy $ Set.insert hvx hvs) rs
            (Nothing, Just _) -> mergeVertices xss (g, lp) x y Pi
            (lpleq, lpeq) -> -- Decide run.
              case sequence [lpleq, lpeq] of
                Nothing -> [] -- Backtrack: Violation of triangle inequality.
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        _ -> [] -- Backtrack: Invalid run.

-- v is first in run, v' is last (counterclockwise rotation/run around graph)
-- vertex (min v v') is kept, while (max v v') is discarded.
mergeVertices :: (VertexTypeSet, VertexTypeSet) -> (TilingGraph, ConvexPolytope AlgebraicNumber) -> Vertex -> Vertex -> ExteriorAngle -> [(TilingGraph, Set HalfInDirection, ConvexPolytope AlgebraicNumber)]
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
  where
    renameVertex :: Vertex -> Vertex -> Corner -> Corner
    renameVertex w w' (Corner ea (s1, z1) ia (s2, z2)) =
      let swap z = if z == w then w' else z
      in Corner ea (s1, swap z1) ia (s2, swap z2)

    -- Returns corner list, completed if possible.
    completeVertex :: [Corner] -> [Corner]
    completeVertex (css@((Corner Unknown e1 ia e2):is)) =
      let vt = vertexType css
          ea = if vt `member` snd xss then
                  case length (filter (\c -> exteriorAngle c == Pi) css) of
                    0 -> Pi
                    1 -> Zero
                    _ -> error "Impossible: Multiple π angles!"
                else if vt `member` fst xss then Zero
                else Unknown
      in (Corner ea e1 ia e2):is
    completeVertex css = css

pickIncompleteVertex :: TilingGraph -> (VertexTypeSet, VertexTypeSet) -> Set HalfInDirection -> Vertex
pickIncompleteVertex g xss hv =
  let incompleteVertices = Map.keys $ Map.filter (not . isVertexComplete) g
      runs = exteriorRuns g
      weighted =
        [(v, v) | v <- incompleteVertices] ++
        [(v, w) | run <- runs,
                       let bends = runBends run,
                       length bends == 1,
                       let (x, y) = runEnds run,
                       -- let w = leastVertexAlongRun run,
                       let w = if numberOfVerticesAlongRun run == 4 then leastVertexAlongRun run else min x y,
                       v <- [x, y],
                       v `member` halfVertices,
                       v `elem` incompleteVertices] ++
        [(v, w) | run <- runs,
                       let bends = runBends run,
                       length bends == 2,
                       let w = leastVertexAlongRun run,
                       let (x, y) = runEnds run,
                       v <- [x, y],
                       v `member` halfVertices,
                       v `elem` incompleteVertices]
      ranked = fst <$> sortOn snd weighted
      topFive = take (min 5 $ length ranked) ranked -- keep focus.
      weighted' = [(v, w) | v <- topFive, let w = vertexScore v]
  in fst $ minBy snd weighted'
  where
    halfVertices :: Set Vertex
    halfVertices = Set.map fst hv

    vertexScore :: Vertex -> Integer
    vertexScore v =
      let vt = vertexType (cornerList g v)
          xs = if v `member` halfVertices then snd xss else fst xss
          vts = compatibleVertexTypes xs vt
          vts' = [sum (vt' |-| vt) | vt' <- elems vts]
      in genericLength vts' * maximum vts'

    numberOfVerticesAlongRun :: Run -> Integer
    numberOfVerticesAlongRun (R _ _ Nothing) = 1
    numberOfVerticesAlongRun (R _ _ (Just (Edge _ r))) = 1 + numberOfVerticesAlongRun r

-- Assumes all possible completions performed.
-- Assumptions:
--  • lp is non-empty (this is ensured by type system and ConvexPolytope impl)
--  • The corrected vertex type of every complete vertex lies in xs.
--  • The corrected vertex type of every non-complete vertex "strictly" "respects" xs (i.e is compatible but not immediately completable).
--  • There are no unchecked exterior (pi/empty) angles.
backtrack :: [Type] -> (VertexTypeSet, VertexTypeSet) -> (TilingGraph, Set HalfInDirection, ConvexPolytope AlgebraicNumber) -> [(TilingGraph, ConvexPolytope AlgebraicNumber, Vector Rational)]
backtrack compatibleTypes xss (g, hv, lp) =
  do -- Situation: We will have to add another tile.
    guard $ all (\(T tname _ tlp) -> if affineSubspace lp `subset` affineSubspace (fromRationalConvexPolytope tlp) then traceShow ("found type", tname) False else True) compatibleTypes
    let incompleteVertex = pickIncompleteVertex g xss hv
    let maxVertexId = fst $ Map.findMax g -- initial 5.
    direction <- if isVertexValidHalfVertex xss (cornerList g incompleteVertex) then [id, Tuple.swap] else [id]
    orientation <- orientations -- Pick direction of new pentagon.
    let anotherTile = pentagonGraph maxVertexId orientation
    let disconnectedGraph = Map.unionWith (\_ _ -> error "Vertex collision!") g anotherTile
    corner <- interiorAngles
    let cornerVertexId = fst $ fromJust $ minWhere (\(_, cs) -> any (\c -> interiorAngle c == corner) cs) anotherTile
    let (v1, v2) = direction (cornerVertexId, incompleteVertex)
    (g', hv', lp') <- mergeVertices xss (disconnectedGraph, lp) v1 v2 Zero
    -- All possible completions will be handled inside 'mergeVertices'.
    let ls = approximateLengths lp'
    (g', lp', ls) : backtrack compatibleTypes xss (g', hv', lp')

halfVertexTypes :: VertexTypeSet -> VertexTypeSet
halfVertexTypes xs =
  let withEvenValues = Set.filter (\x -> all (\v -> v `mod` 2 == 0) x) xs
  in Set.map (\x -> [v `div` 2 | v <- x]) withEvenValues

exhaustiveSearch :: VertexTypeSet -> Vector Rational -> ([(TilingGraph, ConvexPolytope AlgebraicNumber, Vector Rational)])
exhaustiveSearch xs alpha =
  let s = angleSum (traceShow alpha alpha)
      compatibleTypes = [t | t@(T _ cvts _) <- knownTypes, all (`elem` xs) cvts]
      (ps, q) = commonDenominator s
      r = cosineFieldExtension q
      sineConstraint = HP [algebraicNumber r (sinePoly p) | p <- ps] 0
      cosineConstraint = HP [algebraicNumber r (cosinePoly p) | p <- ps] 0
      g = pentagonGraph 0 CounterClockwise
      lp = do
        ass <- foldM intersectWithHyperPlane (space 5) [(HP [1, 1, 1, 1, 1] 1), sineConstraint, cosineConstraint]
        boundedConvexPolytope Strict ass [
            constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
            constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
            constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
            constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
            constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
          ] -- (0, 1)^5
      xss = (xs, halfVertexTypes xs)
  in case traceShow (xs, alpha, s, [name | T name _ _ <- compatibleTypes]) lp of
    Nothing -> []
    Just lp' -> backtrack compatibleTypes xss (g, empty, lp')

pentagonGraph :: Vertex -> Orientation -> TilingGraph
pentagonGraph w orientation =
  fromList [(i + w, offsetCorner <$> corner i) | i <- [1..5]]
  where
    prev :: Integer -> Integer
    prev n = if n == 1 then 5 else n - 1
    next :: Integer -> Integer
    next n = if n == 5 then 1 else n + 1
    toAngle :: Integer -> InteriorAngle
    toAngle n = interiorAngles ! n
    toLength :: Integer -> Length
    toLength n = lengths ! n
    corner :: Integer -> [Corner]
    corner n = case orientation of
      CounterClockwise -> [Corner Unknown (toLength (next n), next n) (toAngle n) (toLength n, prev n)]
      ClockWise -> [Corner Unknown (toLength n, prev n) (toAngle n) (toLength (next n), next n)]
    offsetCorner :: Corner -> Corner
    offsetCorner (Corner ea (l1, v1) ia (l2, v2)) = (Corner ea (l1, v1 + w) ia (l2, v2 + w))

angleSum :: Vector Rational -> Vector Rational
angleSum as = [fromInteger i - 1 - (sum $ genericTake (i - 1) as) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)

approximateLengths :: ConvexPolytope AlgebraicNumber -> Vector Rational
approximateLengths lp =
  let lengthsExtr = elems $ extremePoints lp
  in map (approximate 0.00001) $ (1 / (fromInteger $ genericLength lengthsExtr)) |*| (foldl (|+|) (zero 5) lengthsExtr)

instance JSON Corner where
  toJSON (Corner ea (l1, v1) ia (l2, v2)) =
    jsonObject [
        ("ea", toJSON ea),
        ("l1", toJSON l1),
        ("v1", show $ toJSON v1),
        ("ia", toJSON ia),
        ("l2", toJSON l2),
        ("v2", show $ toJSON v2)
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
