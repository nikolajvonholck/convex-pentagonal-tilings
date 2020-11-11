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

import GoodSubsets (VectorTypeSet, recurse, inflate, permutations, rotationsAndReflections)
import Data.Set (Set, empty, elems, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import TilingGraph (TilingGraph, exhaustiveSearch, planarize)
import Utils (enumerate)
import Vector (Vector)
import JSON
import ConvexPolytope (ConvexPolytope, extremePoints)
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.List (genericIndex)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["goodsubsets"] -> mainGoodSubsets
    ["server"] -> mainServer
    x -> putStrLn $ "Invalid arguments: " ++ show x

mainGoodSubsets :: IO ()
mainGoodSubsets = do
  putStrLn "Will determine all non-empty maximal good sets..."
  let maximalSets = recurse
  let maximalGoodSets = Map.filter id maximalSets
  let nonEmptyMaximalGoodSets = Map.keysSet maximalGoodSets \\ singleton empty
  print $ size nonEmptyMaximalGoodSets -- 193 √
  let allPermutations = unions $ Set.map (fromList . permutations) nonEmptyMaximalGoodSets
  print $ size allPermutations -- 3495 √
  let representatives = unique (toList allPermutations) empty
  print $ length representatives -- 371 √
  putStrLn "Will compare with Michael Rao's results..."
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map (inflate . fromList) raoGenerators of
        Nothing -> error "Failed to compute Michael Rao's maximal good subsets."
        (Just goodSubsets) -> map fst goodSubsets
  putStrLn "Have we found the same maximal good subsets?"
  print $ (raoGoodSubsets `isSubsetOf` representatives) && (representatives `isSubsetOf` raoGoodSubsets)
  where
    isSubsetOf :: [VectorTypeSet] -> [VectorTypeSet] -> Bool
    a `isSubsetOf` b = all (\vs -> any (`elem` b) $ rotationsAndReflections vs) a
    unique :: [VectorTypeSet] -> Set VectorTypeSet -> [VectorTypeSet]
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList $ rotationsAndReflections vs)

mainServer :: IO ()
mainServer = do
  lists <- backtrackings
  startServer $ server $ makeResponder lists

backtrackings :: IO (Map Integer (Vector Rational, [(TilingGraph, ConvexPolytope Rational, Vector Rational)]))
backtrackings = do
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map (inflate . fromList) raoGenerators of
        Nothing -> error "Failed to compute Michael Rao's maximal good subsets."
        (Just goodSubsets) -> goodSubsets
  return $ Map.fromList $ [(i, (alpha, exhaustiveSearch compat alpha)) | (i, (compat, angleCP)) <- enumerate raoGoodSubsets, let points = elems (extremePoints angleCP), points /= [], let alpha = head points]

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

makeResponder :: (Map Integer (Vector Rational, [(TilingGraph, ConvexPolytope Rational, Vector Rational)])) -> Integer -> Integer -> String
makeResponder lists i k =
  case lists !? i of
    Nothing -> "Good subset not found: " ++ show i
    Just (alpha, backtracks) ->
      let (g, lp, lengths) = backtracks `genericIndex` k
      in jsonObject [
        ("graph", toJSON (planarize alpha lengths g)),
        ("lp", toJSON lp)
      ]
