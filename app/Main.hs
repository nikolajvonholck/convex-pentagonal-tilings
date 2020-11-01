module Main where

import GoodSubsets (inflate)
import TilingGraph (exhaustiveSearch)

import Data.Set (fromList)

import Control.Monad (forM_)

main :: IO ()
main = do
  contents <- readFile "data/michael-rao-good-subsets.txt"
  let raoGenerators = read contents :: [[[Integer]]]
  let raoGoodSubsets = case sequence $ map (inflate . fromList) raoGenerators of
        Nothing -> error "Failed to compute Michael Rao's maximal good subsets."
        (Just goodSubsets) -> goodSubsets
  forM_ raoGoodSubsets $ \(compat, angleCP) ->
    case exhaustiveSearch compat angleCP of
      (x:_) -> putStrLn $ "Zero-dimensional: " ++ show x
      [] -> putStrLn "Multi-dimensional."
