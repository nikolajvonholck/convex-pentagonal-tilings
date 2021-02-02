{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Network.Wai (responseLBS, pathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (forM_)

import GoodSet (VertexTypeSet, goodSets, inflate, permutations, rotationsAndReflections)
import Data.Set (Set, empty, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import TilingGraph (TilingGraph, exhaustiveSearch, Pentagon(..))
import Utils (enumerate)
import JSON
import ConvexPolytope (ConvexPolytope, affineSubspace)
import AffineSubspace (dimension)
import qualified Data.Map as Map
import Data.Map (Map, (!?), toAscList)
import Data.List (genericIndex)
import Data.Maybe (fromJust)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["good-sets"] -> mainGoodSets
    ["exhaustive-search"] -> mainExhaustiveSearch
    ["server"] -> mainServer
    x -> putStrLn $ "Invalid arguments: " ++ show x

mainGoodSets :: IO ()
mainGoodSets = do
  putStrLn "Will determine all non-empty, relevant, maximal good sets..."
  forM_ [3..8] $ \n -> do
      putStrLn $ "Considering dimension: " ++ show n
      let maximalGoodSetsN = goodSets n
      let nonEmptyMaximalGoodSetsN = maximalGoodSetsN \\ singleton empty
      -- For n = 5 we expect to get 193, 3495 and 371, respectively.
      putStrLn $ "Found: " ++ (show $ size nonEmptyMaximalGoodSetsN)
      let allPermutationsN = unions $ Set.map (fromList . (permutations n)) nonEmptyMaximalGoodSetsN
      putStrLn $ "All permutations: " ++ (show $ size allPermutationsN)
      let representativesN = unique (toList allPermutationsN) empty
      putStrLn $ "Ignoring symmetries: " ++ (show $ length representativesN)
      forM_ (partitionByDimensionality n representativesN) $ \(d, vs') -> do
        putStrLn $ "Dimensionality " ++ show d ++ ": " ++ (show $ length vs')
      when (n == 5) $ do
        putStrLn "Will compare with Michael Rao's results..."
        raoGoodSets <- map fst <$> loadGoodSetsByRao
        putStrLn "Have we found the same maximal good subsets?"
        let sameResults = (raoGoodSets `isSubsetOf` representativesN) && (representativesN `isSubsetOf` raoGoodSets)
        putStrLn $ if sameResults then "Yes" else "No"
  where
    isSubsetOf :: [VertexTypeSet] -> [VertexTypeSet] -> Bool
    a `isSubsetOf` b = all (\vs -> any (`elem` b) $ rotationsAndReflections 5 vs) a

    unique :: [VertexTypeSet] -> Set VertexTypeSet -> [VertexTypeSet]
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList $ rotationsAndReflections 5 vs)

    partitionByDimensionality :: Integer -> [VertexTypeSet] -> [(Integer, [VertexTypeSet])]
    partitionByDimensionality n vs =
      toAscList $ foldl (\s v -> Map.insertWith (++) (dimension $ affineSubspace $ snd $ fromJust $ inflate n v) [v] s) Map.empty vs

mainServer :: IO ()
mainServer = do
  tracks <- backtrackings <$> loadGoodSetsByRao
  startServer . server . makeResponder $ tracks

mainExhaustiveSearch :: IO ()
mainExhaustiveSearch = do
  tracks <- backtrackings <$> loadGoodSetsByRao
  forM_ (toAscList tracks) $ \(i, track) -> do
    print i
    print $ length track

loadGoodSetsByRao :: IO [(VertexTypeSet, ConvexPolytope Rational)]
loadGoodSetsByRao = do
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  return $ case sequence $ ((inflate 5) . fromList) <$> raoGenerators of
    Nothing -> error "Failed to load Michael Rao's good sets."
    Just sets -> sets

backtrackings :: [(VertexTypeSet, ConvexPolytope Rational)] -> Map Integer [(TilingGraph, ConvexPolytope Rational, Pentagon)]
backtrackings raoGoodSets =
  Map.fromList [(i, exhaustiveSearch compat angleCP) | (i, (compat, angleCP)) <- enumerate raoGoodSets]

startServer :: Application -> IO ()
startServer app = do
  let port = 3333
  putStrLn $ "Listening on port " ++ show port
  run port app

server :: (Integer -> Integer -> String) -> Application
server res req respond = respond $
    case map T.unpack (pathInfo req) of
      [i, k] ->
        let params = do
              i' <- readMaybe i
              k' <- readMaybe k
              return (i', k')
        in case params of
          Just (i', k') -> responseLBS status200 [(hContentType, "application/json"), ("Access-Control-Allow-Origin", "*")] $ LBS.pack $ res i' k'
          Nothing -> responseLBS status404 [(hContentType, "text/plain")] "Invalid input"
      _ -> responseLBS status200 [(hContentType, "text/plain")] $ "Convex pentagonal tiling server."

makeResponder :: Map Integer [(TilingGraph, ConvexPolytope Rational, Pentagon)] -> Integer -> Integer -> String
makeResponder lists i k =
  case lists !? i of
    Nothing -> "Good subset not found: " ++ show i
    Just backtracks ->
      let (g, lp, Pentagon as ls) = backtracks `genericIndex` k
          approxAngles = fromRational <$> as :: [Double]
          approxLengths = fromRational <$> ls :: [Double]
      in jsonObject [
        ("graph", toJSON (Map.map snd g)),
        ("linearProgram", toJSON lp),
        ("angles", toJSON approxAngles),
        ("lengths", toJSON approxLengths)
      ]
