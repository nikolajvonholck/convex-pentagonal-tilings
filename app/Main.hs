{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (responseLBS, pathInfo, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Text.Read (readMaybe)

import GoodSubsets (inflate)
import TilingGraph (TilingGraph, exhaustiveSearch, planarize)
import Utils (enumerate)
import Data.Set (fromList, elems)
import Vector (Vector)
import JSON
import ConvexPolytope (ConvexPolytope, extremePoints)
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.List (genericIndex)

main :: IO ()
main = do
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
