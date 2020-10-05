module TilingGraph (sides, interiorAngles, isVertexValid, canCompleteFullVertex, completeFullVertex, canCompleteHalfVertex, completeHalfVertex, mergeVertices, runs, isRunValid ) where

import Vector ((|*|))
import GoodSubsets (VectorType, VectorTypeSet)

-- import qualified Data.Set as Set
import Data.List (genericLength)
import Data.Set (member)
import qualified Data.Map.Strict as Map
import Data.Map (Map, insert, delete)

data Vertex = Vertex Integer deriving (Show, Eq, Ord)

data InteriorAngle = AOne | ATwo | AThree | AFour | AFive deriving (Show, Eq)
data ExteriorAngle = Unknown | Zero | Pi deriving (Show, Eq)
data Angle = Int InteriorAngle | Ext ExteriorAngle deriving (Show, Eq)
data Side = EOne | ETwo | EThree | EFour | EFive deriving (Show, Eq)

data VertexInfo = VertexInfo Angle Vertex Side
angle :: VertexInfo -> Angle
angle (VertexInfo a _ _) = a
-- vertex :: VertexInfo -> Vertex
-- vertex (VertexInfo _ v _) = v
-- side :: VertexInfo -> Side
-- side (VertexInfo _ _ s) = s

sides :: [Side]
sides = [EOne, ETwo, EThree, EFour, EFive]

interiorAngles :: [InteriorAngle]
interiorAngles = [AOne, ATwo, AThree, AFour, AFive]

-- nextVertex, map from Vertex to edges with sattelite info: edge comes after angle in counterclockwise rotation.
type TilingGraphMap = Map Vertex [VertexInfo]
-- type TilingGraph = (TilingGraphMap, Vertex)

isVertexComplete :: [VertexInfo] -> Bool
isVertexComplete [] = error "Empty vertex info."
isVertexComplete ((VertexInfo (Ext Unknown) _ _):_) = True
isVertexComplete _ = False
-- isVertexComplete (i:_) = angle i /= Ext Unknown
-- isVertexComplete = all ((/=Ext Unknown) . angle)

vectorType :: [VertexInfo] -> VectorType
vectorType is = [genericLength $ filter ((Int a==) . angle) is | a <- interiorAngles]

isVertexHalf :: [VertexInfo] -> Bool
isVertexHalf = any ((Ext Pi==) . angle)

correctedVectorType :: [VertexInfo] -> VectorType
correctedVectorType is =
  let c = if isVertexHalf is then 2 else 1
  in c |*| vectorType is

respects :: VectorType -> VectorType -> Bool
vs `respects` ws = all (\(v, w) -> v <= w) $ zip vs ws

isVertexValid :: VectorTypeSet -> [VertexInfo] -> Bool
isVertexValid xs is =
  length (filter ((Ext Pi==) . angle) is) <= 1 &&
    let cvt = correctedVectorType is
    in if isVertexComplete is
      then cvt `member` xs
      else any (cvt `respects`) xs

canCompleteFullVertex :: VectorTypeSet -> [VertexInfo] -> Bool
canCompleteFullVertex xs is =
  (not $ isVertexComplete is) && correctedVectorType is `member` xs

completeFullVertex :: [VertexInfo] -> [VertexInfo]
completeFullVertex [] = error "Empty vertex info."
completeFullVertex ((VertexInfo (Ext Unknown) v s):is) = (VertexInfo (Ext Zero) v s):is
completeFullVertex _ = error "First angle of incomplete vertex should be unknown."

canCompleteHalfVertex :: VectorTypeSet -> [VertexInfo] -> Bool
canCompleteHalfVertex xs is =
  (not $ isVertexComplete is) && 2 |*| vectorType is `member` xs

completeHalfVertex :: [VertexInfo] -> [VertexInfo]
completeHalfVertex [] = error "Empty vertex info."
completeHalfVertex ((VertexInfo (Ext Unknown) v s):is) = (VertexInfo (Ext Pi) v s):is
completeHalfVertex _ = error "First angle of incomplete vertex should be unknown."

-- v is first in run, v' is last (counterclockwise rotation/run around graph)
-- vertex v is kept, v' is discarded.
mergeVertices :: TilingGraphMap -> Vertex -> Vertex -> ExteriorAngle -> TilingGraphMap
mergeVertices g v v' a =
  let iss = case (vertexInfo g v, vertexInfo g v') of
        ((VertexInfo (Ext Unknown) w s):is, (VertexInfo (Ext Unknown) w' s'):is') ->
          ((VertexInfo (Ext Unknown) w' s'):is') ++ ((VertexInfo (Ext a) w s):is)
        _ -> error "Merging vertices must be incomplete."
      g' = insert v iss $ delete v' g
  in Map.map (map (\(VertexInfo a' w s) -> VertexInfo a' (if w == v' then v else w) s)) g'

vertexInfo :: TilingGraphMap -> Vertex -> [VertexInfo]
vertexInfo g v = case Map.lookup v g of
  Just info -> info
  Nothing -> error "Could not find vertex in graph."

-- wrapInfo :: [VertexInfo] -> [VertexInfo]
-- wrapInfo [] = error "Vertex info empty."
-- wrapInfo (i:is) = i:is ++ [i]

exteriorFaceFromIncompleteVertex :: TilingGraphMap -> Vertex -> [(Vertex, ExteriorAngle)]
exteriorFaceFromIncompleteVertex g v = (v, Unknown) : exterior v
  where
    exterior :: Vertex -> [(Vertex, ExteriorAngle)]
    exterior w = case vertexInfo g w of
      ((VertexInfo (Ext a) w' _):_) ->
        (w, a) : if w' == v then [(v, Unknown)] else exterior w'
      _ -> error "Exterior face has interior angle."

-- Assumed to begin and end with an unknown angle.
type Run = [(Vertex, ExteriorAngle)]

runs :: TilingGraphMap -> Vertex -> [Run]
runs g v = collectRuns $ exteriorFaceFromIncompleteVertex g v
  where
    collectRuns :: [(Vertex, ExteriorAngle)] -> [Run]
    collectRuns [] = error "Can not collect runs from empty list."
    collectRuns [_] = error "Can not collect runs from list with only one item."
    collectRuns (ex:ex':exs) = case snd ex of
      Unknown -> case snd ex' of
        Unknown -> collectRuns $ ex':exs -- Skip first unknown angle.
        _ -> let (run, exs') = collectRun $ ex':exs -- Collect rest of run.
             in (ex':run):collectRuns exs'
      _ -> error "Can only collect runs from list starting with unknown angle."

    collectRun :: [(Vertex, ExteriorAngle)] -> (Run, [(Vertex, ExteriorAngle)])
    collectRun [] = error "Can not collect empty run."
    collectRun (ex:exs) = case snd ex of
      Unknown -> ([ex], ex:exs) -- Has reached end of run
      _ -> let (run, exs') = collectRun exs in (ex:run, exs')

isRunValid :: Run -> Bool
isRunValid run = length (filter ((Zero==) . snd) run) <= 2
