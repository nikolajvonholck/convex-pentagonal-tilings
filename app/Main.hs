{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.TimeIt
import System.Environment
import Network.Wai (responseLBS, pathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (forM_)

import GoodSet (VertexTypeSet, goodSets, inflate, permutations, rotationsAndReflections, ignoringSymmetries, partitionByDimensionality)
import Data.Set (empty, size, fromList, unions, elems)
import qualified Data.Set as Set
import TilingGraph (TilingGraph, backtrackingSearch, Pentagon(..))
import Type (Type(..))
import Utils (enumerate, (!))
import JSON
import ConvexPolytope (ConvexPolytope)
import qualified Data.Map as Map -- Map should be non-strict.
import Data.Map (Map, (!?), toAscList)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["good-sets"] -> mainGoodSets
    ["backtracking-search"] -> mainBacktrackingSearch
    ["server"] -> mainServer
    x -> putStrLn $ "Invalid arguments: " ++ show x

mainGoodSets :: IO ()
mainGoodSets = do
  putStrLn "Will determine all relevant maximal good sets..."
  forM_ [3..8] $ \n -> do
      putStrLn $ "Considering dimension: " ++ show n
      let relevantMaximalGoodSetsN = goodSets n
      timeIt $ putStrLn $ "Found: " ++ (show $ size relevantMaximalGoodSetsN)
      let allPermutationsN = unions $ Set.map (fromList . (permutations n)) relevantMaximalGoodSetsN
      putStrLn $ "All permutations: " ++ (show $ size allPermutationsN)
      let representativesN = ignoringSymmetries n (elems allPermutationsN) empty
      putStrLn $ "Ignoring symmetries: " ++ (show $ length representativesN)
      forM_ (partitionByDimensionality n representativesN) $ \(d, vs') -> do
        putStrLn $ "Dimensionality " ++ show d ++ ": " ++ (show $ length vs')
      forM_ (enumerate representativesN) $ \(i, goodSet) -> do
        print i
        print $ elems goodSet
      when (n == 5) $ do
        let a `isSubsetOf` b = all (\vs -> any (`elem` b) $ rotationsAndReflections n vs) a
        putStrLn "Will compare with Michael Rao's results..."
        raoGoodSets <- map fst <$> loadGoodSetsByRao
        putStrLn "Have we found the same maximal good subsets?"
        let sameResults = (raoGoodSets `isSubsetOf` representativesN) && (representativesN `isSubsetOf` raoGoodSets)
        putStrLn $ if sameResults then "Yes" else "No"
        putStrLn $ "Relevant maximal good sets according to Rao's numbering:"
        forM_ (enumerate raoGoodSets) $ \(i, raoGoodSet) -> do
          print i
          print $ elems raoGoodSet

mainServer :: IO ()
mainServer = do
  tracks <- backtrackings <$> loadGoodSetsByRao
  startServer . server . makeResponder $ tracks

mainBacktrackingSearch :: IO ()
mainBacktrackingSearch = do
  tracks <- backtrackings <$> loadGoodSetsByRao
  forM_ (toAscList tracks) $ \(i, (goodSet, track)) -> do
    putStrLn $ "Backtracking search for good set: " ++ show i
    print $ elems goodSet
    let foundTypes = catMaybes [knownType | (_, _, _, knownType) <- track]
    let foundTypeNames = fromList [ name | T (name, _) _ _  <- foundTypes]
    putStrLn "Found types:"
    putStrLn $ intercalate ", " $ elems foundTypeNames
    putStrLn $ "Total number of steps: " ++ (show $ length $ track)

loadGoodSetsByRao :: IO [(VertexTypeSet, ConvexPolytope Rational)]
loadGoodSetsByRao = do
  contents <- readFile "data/michael-rao-good-sets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  return $ case sequence $ ((inflate 5) . fromList) <$> raoGenerators of
    Nothing -> error "Failed to load Michael Rao's good sets."
    Just sets -> sets

backtrackings :: [(VertexTypeSet, ConvexPolytope Rational)] -> Map Integer (VertexTypeSet, [(TilingGraph, ConvexPolytope Rational, Pentagon, Maybe Type)])
backtrackings raoGoodSets =
  Map.fromList [(i, (goodSet, backtrackingSearch goodSet angleCP)) | (i, (goodSet, angleCP)) <- enumerate raoGoodSets]

startServer :: Application -> IO ()
startServer app = do
  let port = 3001
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

makeResponder :: Map Integer (VertexTypeSet, [(TilingGraph, ConvexPolytope Rational, Pentagon, Maybe Type)]) -> Integer -> Integer -> String
makeResponder lists i k =
  case lists !? i of
    Nothing -> "Good set not found: " ++ show i
    Just (_, backtracks) ->
      let (g, lp, Pentagon as ls, _) = backtracks ! k
          approxAngles = fromRational <$> as :: [Double]
          approxLengths = fromRational <$> ls :: [Double]
      in jsonObject [
        ("graph", toJSON (Map.map snd g)),
        ("linearProgram", toJSON lp),
        ("angles", toJSON approxAngles),
        ("lengths", toJSON approxLengths)
      ]
