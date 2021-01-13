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

import GoodSubsets (VectorTypeSet, goodSets, inflate, permutations, rotationsAndReflections)
import AlgebraicNumber (AlgebraicNumber)
import Data.Set (Set, empty, elems, singleton, (\\), size, toList, fromList, unions, member, union)
import qualified Data.Set as Set
import TilingGraph (TilingGraph, exhaustiveSearch)
import Utils (enumerate)
import Vector (Vector)
import JSON
import ConvexPolytope (ConvexPolytope, extremePoints)
import qualified Data.Map as Map
import Data.Map (Map, (!?), toAscList)
import Data.List (genericIndex)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["goodsubsets"] -> mainGoodSubsets
    ["server"] -> mainServer
    ["exhaustiveSearch"] -> mainExhaustiveSearch
    x -> putStrLn $ "Invalid arguments: " ++ show x

mainGoodSubsets :: IO ()
mainGoodSubsets = do
  putStrLn "Will determine all non-empty maximal good sets..."
  let maximalGoodSets = goodSets 5
  let nonEmptyMaximalGoodSets = maximalGoodSets \\ singleton empty
  print $ size nonEmptyMaximalGoodSets -- 193 √
  let allPermutations = unions $ Set.map (fromList . (permutations 5)) nonEmptyMaximalGoodSets
  print $ size allPermutations -- 3495 √
  let representatives = unique (toList allPermutations) empty
  print $ length representatives -- 371 √
  putStrLn "Will compare with Michael Rao's results..."
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map ((inflate 5) . fromList) raoGenerators of
        Nothing -> error "Failed to compute Michael Rao's maximal good subsets."
        (Just goodSubsets) -> map fst goodSubsets
  putStrLn "Have we found the same maximal good subsets?"
  print $ (raoGoodSubsets `isSubsetOf` representatives) && (representatives `isSubsetOf` raoGoodSubsets)
  where
    isSubsetOf :: [VectorTypeSet] -> [VectorTypeSet] -> Bool
    a `isSubsetOf` b = all (\vs -> any (`elem` b) $ rotationsAndReflections 5 vs) a
    unique :: [VectorTypeSet] -> Set VectorTypeSet -> [VectorTypeSet]
    unique [] _ = []
    unique (vs:vss) except =
      if vs `member` except
        then unique vss except
        else vs : (unique vss $ union except $ fromList $ rotationsAndReflections 5 vs)

mainServer :: IO ()
mainServer = do
  lists <- backtrackings
  startServer $ server $ makeResponder lists

mainExhaustiveSearch :: IO ()
mainExhaustiveSearch = do
  lists <- backtrackings
  let zeroDimensionalTraces = [(i, trace) | (i, trace) <- toAscList lists, i >= 121]
  forM_ zeroDimensionalTraces $ \(i, (_, trace)) -> do
    print i
    print $ length trace

backtrackings :: IO (Map Integer (Vector Rational, [(TilingGraph, ConvexPolytope AlgebraicNumber, Vector Rational)]))
backtrackings = do
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map ((inflate 5) . fromList) raoGenerators of
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

makeResponder :: (Map Integer (Vector Rational, [(TilingGraph, ConvexPolytope AlgebraicNumber, Vector Rational)])) -> Integer -> Integer -> String
makeResponder lists i k =
  case lists !? i of
    Nothing -> "Good subset not found: " ++ show i
    Just (alpha, backtracks) ->
      let (g, lp, ls) = backtracks `genericIndex` k
          approxAlpha = map fromRational alpha :: [Double]
          approxLengths = map fromRational ls :: [Double]
      in jsonObject [
        ("graph", toJSON g),
        ("linearProgram", toJSON lp),
        ("lengths", toJSON approxLengths),
        ("angles", toJSON approxAlpha)
      ]
