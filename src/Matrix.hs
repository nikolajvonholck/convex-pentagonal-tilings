module Matrix (Matrix, reducedEchelonForm, nullSpaceBasis) where

import Utils ((!), enumerate, findIndex, delta)
import Vector (Vector, dimension, unit, (|*|), dot)

import Data.List (transpose, find, (\\))
import Data.Maybe (catMaybes)

type Matrix = Vector (Vector Rational)

mRows :: Matrix -> Integer
mRows = dimension

nCols :: Matrix -> Integer
nCols = dimension . head

dimensions :: Matrix -> (Integer, Integer)
dimensions xss = (mRows xss, nCols xss)

(|.|) :: Matrix -> Matrix -> Matrix
m |.| n = [[row `dot` col | col <- transpose n] | row <- m]

matrix :: Integer -> Integer -> (Integer -> Integer -> Rational) -> Matrix
matrix m n entry = [[entry i j | j <- [1..n]] | i <- [1..m]]

squareMatrix :: Integer -> (Integer -> Integer -> Rational) -> Matrix
squareMatrix n = matrix n n

swapRows :: Integer -> Integer -> Matrix -> Matrix
swapRows i j xss =
  let m = mRows xss
      elemMatrix = squareMatrix m (\r c ->
        if r == i then delta j c else
          if r == j then delta i c
          else delta r c)
  in elemMatrix |.| xss

reducedEchelonForm :: Matrix -> Matrix
reducedEchelonForm = reduceFromCol (1, 1)
  where
    reduceFromCol :: (Integer, Integer) -> Matrix -> Matrix
    reduceFromCol (col, topRow) xss =
      let (m, n) = dimensions xss in
        if col > n || topRow > m then xss -- Reduction complete.
        else let column = (transpose xss) ! col
        in case find (\(i, x) -> topRow <= i && x /= 0) (enumerate column) of
            Nothing -> reduceFromCol (col + 1, topRow) xss -- Skip column
            Just (pivotRow, pivot) ->
              let rowOpsMatrix = squareMatrix m (\r c ->
                    if c == pivotRow then if r == pivotRow
                      then 1 / pivot
                      else -column!r / pivot
                    else delta r c)
                  pivotInTop = swapRows topRow pivotRow $ rowOpsMatrix |.| xss
              in reduceFromCol (col + 1, topRow + 1) pivotInTop

nullSpaceBasis :: Matrix -> [Vector Rational]
nullSpaceBasis = makeBasis . reducedEchelonForm
  where
    makeBasis :: Matrix -> [Vector Rational]
    makeBasis xss =
      let n = nCols xss
          pivotIndices = catMaybes $ [findIndex (/=0) row | row <- xss]
          nonPivotIndices = [1..n] \\ pivotIndices
          indicators = [i `elem` pivotIndices | i <- [1..n]]
          negatedXss = [(-1) |*| row | row <- xss]
          units = [unit n i | i <- nonPivotIndices]
          weaved = weave indicators negatedXss units
      in [col | (j, col) <- enumerate (transpose weaved), j `elem` nonPivotIndices]
      where
        weave :: [Bool] -> [Vector a] -> [Vector a] -> [Vector a]
        weave [] _ _ = []
        weave (True:_) [] _ = error "Impossible"
        weave (False:_) _ [] = error "Impossible"
        weave (True:xs) (y:ys) zs  = y : weave xs ys zs
        weave (False:xs) ys (z:zs) = z : weave xs ys zs
