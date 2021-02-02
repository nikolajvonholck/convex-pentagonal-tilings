module TilingGraph (TilingGraph, exhaustiveSearch, Pentagon(..)) where

import Vector (Vector, (|+|), (|-|), (|*|))
import Interval (Interval, interval, begin, end, midpoint, width)
import GoodSet (VertexType, VertexTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space, subset, dimension, coordsInSpace)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope, projectOntoHyperplane, cutHalfSpace, extremePoints, localExtremePoints, fromRationalConvexPolytope, affineSubspace)
import AlgebraicNumber (AlgebraicNumber, algebraicNumber, approximate)
import Trigonometry (cosBound', sinBound')
import ChebyshevPolynomial (commonDenominator, cosineFieldExtension, sinePoly, cosinePoly)
import Type (Type(..), knownTypes)
import Utils ((!), minBy, maxBy, enumerate, replaceAt, zipPedantic)
import JSON

import qualified Data.Set as Set
import Data.Set (Set, empty, member, notMember, elems, union, size)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, insert, delete, fromList)
import Data.Maybe (fromJust, maybeToList, listToMaybe)
import Data.List (find, genericTake, genericLength, sortOn, transpose)
import Control.Monad (guard, foldM)
import qualified Data.Tuple as Tuple

import Debug.Trace (traceShow)

type Vertex = Integer

data InteriorAngle = AngleA | AngleB | AngleC | AngleD | AngleE deriving (Show, Eq)
data ExteriorAngle = Unknown | Zero | Pi deriving (Show, Eq)
data Length = LengthA | LengthB | LengthC | LengthD | LengthE deriving (Show, Eq)
data Direction = CounterClockwise | Clockwise deriving (Show, Eq, Ord)

asRational :: Vector Integer -> Vector Rational
asRational = map fromInteger

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
type VertexInfo = (Set Direction, [Corner])
type TilingGraph = Map Vertex VertexInfo
data Pentagon = Pentagon (Vector Rational) (Vector Rational) deriving (Eq)

isVertexComplete :: [Corner] -> Bool
isVertexComplete ((Corner Unknown _ _ _):_) = False
isVertexComplete _ = True

vertexType :: [Corner] -> VertexType
vertexType cs = [genericLength $ filter (\c -> interiorAngle c == a) cs | a <- interiorAngles]

vertexInfo :: TilingGraph -> Vertex -> VertexInfo
vertexInfo g v = fromJust $ Map.lookup v g

cornerList :: TilingGraph -> Vertex -> [Corner]
cornerList g v = snd $ vertexInfo g v

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

piAngles :: [Corner] -> Int
piAngles cs = length (filter (\c -> exteriorAngle c == Pi) cs)

isHalfVertex :: VertexInfo -> Bool
isHalfVertex (hds, cs) = 0 < piAngles cs + size hds

isValidVertex :: (VertexTypeSet, VertexTypeSet) -> VertexInfo -> Bool
isValidVertex xss (hds, cs) =
  let vt = vertexType cs
  in case piAngles cs + size hds of
      0 -> isCompatibleWith (fst xss) vt -- Must be completed as either half or full.
      1 -> isCompatibleWith (snd xss) vt -- Must be completed as half vertex.
      _ -> False -- Vertex can not be half along multiple lines.

compatibleVertexTypes :: VertexTypeSet -> VertexType -> VertexTypeSet
compatibleVertexTypes xs vt =
  Set.filter (\x -> all (\(v, w) -> v <= w) $ zip vt x) xs

isCompatibleWith :: VertexTypeSet -> VertexType -> Bool
isCompatibleWith xs x = 0 < Set.size (compatibleVertexTypes xs x)

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
  let (v, (_, cs)) = fromJust $ minWhere (not . isVertexComplete . snd . snd) g
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

completeRuns :: (VertexTypeSet, VertexTypeSet) -> (TilingGraph, ConvexPolytope Rational) -> [(TilingGraph, ConvexPolytope Rational)]
completeRuns xss (g'', lp) = completeRuns' g'' $ exteriorRuns g''
  where
    completeRuns' :: TilingGraph -> [Run] -> [(TilingGraph, ConvexPolytope Rational)]
    completeRuns' g [] = [(g, lp)]
    completeRuns' g (r:rs) =
      let (x, y) = runEnds r
      in case map lengthCounts $ runLengths r of
        [_] -> completeRuns' g rs -- No bends to check. Skip this run.
        [la, lb] ->
          let cs = asRational$ la |-| lb -- la < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0), cutHalfSpace lp (constraint ((-1) |*| cs) 0)) of
            (Just _, Nothing, Nothing) ->
              do
                let (hdsx, csx) = vertexInfo g x
                let vix' = (Set.insert Clockwise hdsx, csx)
                guard $ isValidVertex xss vix'
                completeRuns' (Map.insert x vix' g) rs
            (Nothing, Just _, Nothing) -> mergeVertices xss (g, lp) y x Zero
            (Nothing, Nothing, Just _) ->
              do
                let (hdsy, csy) = vertexInfo g y
                let viy' = (Set.insert CounterClockwise hdsy, csy)
                guard $ isValidVertex xss viy'
                completeRuns' (Map.insert y viy' g) rs
            (lpleq, lpeq, lpgeq) -> -- Decide run.
              case sequence [lpleq, lpeq, lpgeq] of
                Nothing -> error "Impossible: There should always be an option"
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        [la, lb, lc] ->
          let cs = asRational $ (la |+| lc) |-| lb -- la + lc < lb
          in case (cutHalfSpace lp (constraint cs 0), projectOntoHyperplane lp (HP cs 0)) of
            (Just _, Nothing) ->
              do
                let (hdsx, csx) = vertexInfo g x
                let (hdsy, csy) = vertexInfo g y
                let vix' = (Set.insert Clockwise hdsx, csx)
                let viy' = (Set.insert CounterClockwise hdsy, csy)
                guard $ isValidVertex xss vix'
                guard $ isValidVertex xss viy'
                let g' = Map.insert y viy' $ Map.insert x vix' g
                completeRuns' g' rs
            (Nothing, Just _) -> mergeVertices xss (g, lp) y x Pi
            (lpleq, lpeq) -> -- Decide run.
              case sequence [lpleq, lpeq] of
                Nothing -> [] -- Backtrack: Violation of triangle inequality.
                Just lps' -> do lp' <- lps'; completeRuns xss (g, lp')
        _ -> [] -- Backtrack: Invalid run.

-- Edges of v are followed by edges of v' in counterclockwise direction.
-- Vertex id (min v v') is kept, while (max v v') is discarded.
mergeVertices :: (VertexTypeSet, VertexTypeSet) -> (TilingGraph, ConvexPolytope Rational) -> Vertex -> Vertex -> ExteriorAngle -> [(TilingGraph, ConvexPolytope Rational)]
mergeVertices xss (g, lp) v v' a =
  do
    let (hds, cs) = vertexInfo g v
    let (hds', cs') = vertexInfo g v'
    let cs'' = case (cs, cs') of
          ((Corner Unknown e1 ia e2):cst, (Corner Unknown e1' ia' e2'):cst') ->
            (Corner Unknown e1 ia e2):cst ++ (Corner a e1' ia' e2'):cst'
          _ -> error "Merging vertices must be incomplete."
    guard $ a == Pi || (CounterClockwise `notMember` hds && Clockwise `notMember` hds')
    let (vKeep, vDiscard) = (min v v', max v v') -- Choose to keep least vertex index.
    let hds'' = Set.delete CounterClockwise hds `union` Set.delete Clockwise hds'
    guard $ isValidVertex xss (hds'', cs'') -- Backtrack if merged vertex can never be completed.
    let vi' = completeVertex (hds'', cs'') -- Attempt to complete vertex. Might return unaltered vertex info.
    let g' = Map.map (\(h, c) -> (h, map (renameVertex vDiscard vKeep) c)) $ insert vKeep vi' $ delete vDiscard g
    completeRuns xss (g', lp) -- There might be completable runs.
  where
    renameVertex :: Vertex -> Vertex -> Corner -> Corner
    renameVertex w w' (Corner ea (s1, z1) ia (s2, z2)) =
      let swap z = if z == w then w' else z
      in Corner ea (s1, swap z1) ia (s2, swap z2)

    -- Returns corner list, completed if possible.
    completeVertex :: VertexInfo -> VertexInfo
    completeVertex (hds, (cs@((Corner Unknown e1 ia e2):cst))) =
      let vt = vertexType cs
          (hds', ea) = if vt `member` snd xss then -- Can be completed as half vertex, so we do that.
                  case piAngles cs of
                    0 -> (empty, Pi)
                    1 -> (empty, Zero)
                    _ -> error "Impossible: Multiple Pi angles!"
                else if vt `member` fst xss then (empty, Zero) -- Can be completed as full vertex, so we do that alternatively.
                else (hds, Unknown) -- No change is made.
      in (hds', (Corner ea e1 ia e2):cst)
    completeVertex _ = error "Impossible: Vertex is always incomplete."

pickIncompleteVertex :: TilingGraph -> (VertexTypeSet, VertexTypeSet) -> Vertex
pickIncompleteVertex g xss =
  let incompleteVertices = Map.keys $ Map.filter (not . isVertexComplete . snd) g
      runs = exteriorRuns g
      weighted =
        [(v, v) | v <- incompleteVertices] ++
        [(v, w) | run <- runs,
                       let bends = runBends run,
                       length bends == 1,
                       let (x, y) = runEnds run,
                       let w = if numberOfVerticesAlongRun run <= 4 then leastVertexAlongRun run else min x y,
                       v <- [x, y],
                       v `elem` incompleteVertices,
                       isHalfVertex $ vertexInfo g v] ++
        [(v, w) | run <- runs,
                       let bends = runBends run,
                       length bends == 2,
                       let w = leastVertexAlongRun run,
                       let (x, y) = runEnds run,
                       v <- [x, y],
                       v `elem` incompleteVertices,
                       isHalfVertex $ vertexInfo g v]
      ranked = fst <$> sortOn snd weighted
      topFive = take 15 ranked -- keep focus.
      weighted' = [(v, w) | v <- topFive, let w = vertexScore v]
  in fst $ minBy snd weighted'
  where
    vertexScore :: Vertex -> Integer
    vertexScore v =
      let (hds, cs) = vertexInfo g v
          vt = vertexType cs
          xs = if isHalfVertex (hds, cs) then snd xss else fst xss
          vts = compatibleVertexTypes xs vt
          vts' = [sum (vt' |-| vt) | vt' <- elems vts]
      in sum [product [1..p] | p <- vts']

    numberOfVerticesAlongRun :: Run -> Integer
    numberOfVerticesAlongRun (R _ _ Nothing) = 1
    numberOfVerticesAlongRun (R _ _ (Just (Edge _ r))) = 1 + numberOfVerticesAlongRun r

-- Assumes all possible completions performed.
-- Assumptions:
--  • lp is non-empty (this is ensured by type system and ConvexPolytope impl)
--  • The corrected vertex type of every complete vertex lies in xs.
--  • The corrected vertex type of every non-complete vertex "strictly" "respects" xs (i.e is compatible but not immediately completable).
--  • There are no unchecked exterior (pi/empty) angles.
backtrack :: (ConvexPolytope Rational -> Bool) -> (ConvexPolytope Rational -> Maybe Pentagon) -> (VertexTypeSet, VertexTypeSet) -> (TilingGraph, ConvexPolytope Rational) -> [(TilingGraph, ConvexPolytope Rational, Pentagon)]
backtrack isKnownType construct xss state = traceShow (xss) backtrack' state
  where
    backtrack' :: (TilingGraph, ConvexPolytope Rational) -> [(TilingGraph, ConvexPolytope Rational, Pentagon)]
    backtrack' (g, lp) = do
      pentagon <- maybeToList $ construct lp
      guard $ not $ isKnownType lp
      -- Situation: We will have to add another tile.
      let v = pickIncompleteVertex g xss
      let (hds, cs) = vertexInfo g v
      (direction, hds') <- if size hds > 1 then error "Invalid vertex!" else
                           if Clockwise `member` hds then [(id, hds)]
                           else if CounterClockwise `member` hds then [(Tuple.swap, hds)]
                           else -- hds is empty
                             if isCompatibleWith (snd xss) (vertexType cs) && piAngles cs == 0 -- Check if might become half vertex.
                                then [(id, hds), (Tuple.swap, Set.insert CounterClockwise hds)]
                                else [(id, hds)]
      let g'' = Map.insert v (hds', cs) g -- Update half vertex status of v.
      orientation <- [CounterClockwise, Clockwise] -- Pick orientation of new pentagon.
      let maxVertexId = fst $ Map.findMax g'' -- initial 5.
      let anotherTile = pentagonGraph maxVertexId orientation
      let disconnectedGraph = Map.unionWith (\_ _ -> error "Vertex id collision!") g'' anotherTile
      corner <- interiorAngles
      let cornerVertexId = fst $ fromJust $ minWhere (\(_, (_, cs')) -> any (\c -> interiorAngle c == corner) cs') anotherTile
      let (v1, v2) = direction (v, cornerVertexId)
      -- All possible completions will be handled inside 'mergeVertices'.
      (g', lp') <- mergeVertices xss (disconnectedGraph, lp) v1 v2 Zero
      (g, lp, pentagon) : backtrack' (g', lp')

halfVertexTypes :: VertexTypeSet -> VertexTypeSet
halfVertexTypes xs =
  let withEvenValues = Set.filter (\x -> all (\v -> v `mod` 2 == 0) x) xs
  in Set.map (\x -> [v `div` 2 | v <- x]) withEvenValues

exhaustiveSearch :: VertexTypeSet -> ConvexPolytope Rational -> [(TilingGraph, ConvexPolytope Rational, Pentagon)]
exhaustiveSearch xs angleCP =
  let compatibleTypes = [t | t@(T _ cvts _) <- knownTypes, all (`elem` xs) cvts]
      xss = traceShow xs (xs, halfVertexTypes xs)
      g = pentagonGraph 0 CounterClockwise
      lp = fromJust $ do
        ass <- intersectWithHyperPlane (space 5) (HP [1, 1, 1, 1, 1] 1)
        boundedConvexPolytope Strict ass [
            constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
            constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
            constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
            constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
            constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
          ] -- (0, 1)^5
      (isKnownType', construct') =
        if dimension (affineSubspace angleCP) == 0
        then -- Decide on algebric field extension of the rationals.
          let alpha = head $ elems $ extremePoints angleCP
              s = angleSum (traceShow alpha alpha)
              (ps, q) = commonDenominator s
              r = cosineFieldExtension q
              sineConstraint = HP [algebraicNumber r (sinePoly p) | p <- ps] 0
              cosineConstraint = HP [algebraicNumber r (cosinePoly p) | p <- ps] 0
              constructor lp' = foldM projectOntoHyperplane (fromRationalConvexPolytope lp') [sineConstraint, cosineConstraint]
              constructableCompatibleTypes = [(t, clp) | t@(T _ _ tlp) <- compatibleTypes, clp <- maybeToList $ constructor tlp]
              isKnownType lp' =
                let alp' = fromJust $ constructor lp' -- Assumes constructible.
                in any (\(T tname _ _, clp) -> if affineSubspace alp' `subset` affineSubspace clp then traceShow ("Found " ++ tname) True else False) constructableCompatibleTypes
              construct lp' = do
                clp <- constructor lp'
                return $ Pentagon alpha (approximateLengths clp)
          in traceShow ("Constructible known types:", [name | (T name _ _, _) <- constructableCompatibleTypes]) (isKnownType, construct)
        else
          let isKnownType lp' = any (\(T tname _ tlp) -> if affineSubspace lp' `subset` affineSubspace tlp then traceShow ("Found " ++ tname) True else False) compatibleTypes
              initialSector = boundingBox (elems $ localExtremePoints angleCP)
              construct lp' = listToMaybe $ constructSectors angleCP lp' initialSector
          in (isKnownType, construct)
  in backtrack isKnownType' construct' (traceShow [name | T name _ _ <- compatibleTypes] xss) (g, lp)

type Sector = [Interval Rational]

boundingBox :: (Ord a) => [Vector a] -> [Interval a]
boundingBox vs =
  let coordLists = transpose vs
      (mins, maxs) = (minimum <$> coordLists, maximum <$> coordLists)
  in interval <$> zipPedantic mins maxs

constructSectors :: ConvexPolytope Rational -> ConvexPolytope Rational -> Sector -> [Pentagon]
constructSectors angleCP lp sector = do
    construction <- maybeToList $ constructSector sector
    if maximum [width i | i <- sector] < sectorSize
      then return construction -- Here we choose to not split sector further.
      else splitSector sector >>= constructSectors angleCP lp
  where
    sectorSize :: Rational
    sectorSize = 1/1024 -- TODO: Adjust.

    splitSector :: Sector -> [Sector]
    splitSector is =
      let (k, i) = maxBy (width . snd) (enumerate is) -- Determine widest dimension.
          m = midpoint i -- Split at midpoint.
      in [replaceAt k i' is | i' <- [interval (begin i, m), interval (m, end i)]]

    constructSector :: Sector -> Maybe Pentagon
    constructSector sector' =
      let bounds = sequence [[begin i, end i] | i <- sector']
          angleBounds = coordsInSpace (affineSubspace angleCP) <$> bounds
          angleBoundingBox = boundingBox angleBounds
          angleSums = angleSum <$> angleBounds
          angleSumBounds = boundingBox angleSums
          sineBounds = sinBound' <$> angleSumBounds
          cosineBounds = cosBound' <$> angleSumBounds
          constraints = [
              constraint (begin <$> cosineBounds) 0,
              constraint ((negate . end) <$> cosineBounds) 0,
              constraint (begin <$> sineBounds) 0,
              constraint ((negate . end) <$> sineBounds) 0
            ]
          (mins, maxs) = (begin <$> angleBoundingBox, end <$> angleBoundingBox)
      in do
        guard $ all (<=1) mins && all (0<=) maxs -- TODO: Consider strict ineqs.
        lp' <- foldM cutHalfSpace lp constraints
        let as = averageVector angleBounds
        let ls = averageVector (elems $ extremePoints lp')
        return $ Pentagon as ls

pentagonGraph :: Vertex -> Direction -> TilingGraph
pentagonGraph w orientation =
  fromList [(i + w, (empty, offsetCorner <$> corner i)) | i <- [1..5]]
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
      Clockwise -> [Corner Unknown (toLength n, prev n) (toAngle n) (toLength (next n), next n)]
    offsetCorner :: Corner -> Corner
    offsetCorner (Corner ea (l1, v1) ia (l2, v2)) = (Corner ea (l1, v1 + w) ia (l2, v2 + w))

angleSum :: Vector Rational -> Vector Rational
angleSum as = [fromInteger i - 1 - (sum $ genericTake (i - 1) as) | i <- [1..5]]

-- Returns (k, v) for minimal key satisfying p.
minWhere :: ((k, v) -> Bool) -> Map k v -> Maybe (k, v)
minWhere p m = find p (Map.toAscList m)

averageVector :: (Fractional a) => [Vector a] -> Vector a
averageVector [] = error "Can not take average of empty list"
averageVector vs =
  let s = foldl1 (|+|) vs
      k = genericLength vs
  in (1 / (fromInteger k)) |*| s

approximateLengths :: ConvexPolytope AlgebraicNumber -> Vector Rational
approximateLengths lp =
  approximate 0.00001 <$> averageVector (elems $ extremePoints lp)

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
