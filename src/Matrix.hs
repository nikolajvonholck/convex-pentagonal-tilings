module Matrix (Matrix, reducedEchelonForm, nullSpaceBasis, matrixVectorProduct, mRows, nCols, squareMatrix, rank, inSpan) where

import Utils ((!), enumerate, findIndex, delta, zipPedantic)
import Vector (Vector, dimension, unit, (|*|), dot, isZero)

import Data.List (transpose, find, (\\))
import Data.Maybe (catMaybes)

type Matrix a = Vector (Vector a)

mRows :: Matrix a -> Integer
mRows = dimension

nCols :: Matrix a -> Integer
nCols = dimension . head

dimensions :: Matrix a -> (Integer, Integer)
dimensions xss = (mRows xss, nCols xss)

(|.|) :: Num a => Matrix a -> Matrix a -> Matrix a
m |.| n = [[row `dot` col | col <- transpose n] | row <- m]

matrixVectorProduct :: Num a => Matrix a -> Vector a -> Vector a
matrixVectorProduct m v = map (`dot` v) m

matrix :: Num a => Integer -> Integer -> (Integer -> Integer -> a) -> Matrix a
matrix m n entry = [[entry i j | j <- [1..n]] | i <- [1..m]]

squareMatrix :: Num a => Integer -> (Integer -> Integer -> a) -> Matrix a
squareMatrix n = matrix n n

swapRows :: Num a => Integer -> Integer -> Matrix a -> Matrix a
swapRows i j xss =
  let m = mRows xss
      elemMatrix = squareMatrix m (\r c ->
        if r == i then delta j c else
          if r == j then delta i c
          else delta r c)
  in elemMatrix |.| xss

rank :: (Fractional a, Eq a) => Matrix a -> Integer
rank ms = mRows $ takeWhile (not . isZero) $ reducedEchelonForm ms

reducedEchelonForm :: (Fractional a, Eq a) => Matrix a -> Matrix a
reducedEchelonForm [] = []
reducedEchelonForm ms = reduceFromCol (1, 1) ms
  where
    reduceFromCol :: (Fractional a, Eq a) => (Integer, Integer) -> Matrix a -> Matrix a
    reduceFromCol (col, topRow) xss =
      let (m, n) = dimensions xss in
        if col > n || topRow > m then xss -- Reduction complete.
        else let column = (transpose xss) ! col
        in case find (\(i, x) -> topRow <= i && x /= 0) (enumerate column) of
            Nothing -> reduceFromCol (col + 1, topRow) xss -- Skip column.
            Just (pivotRow, pivot) ->
              let rowOpsMatrix = squareMatrix m (\r c ->
                    if c == pivotRow then if r == pivotRow
                      then 1 / pivot
                      else -column!r / pivot
                    else delta r c)
                  pivotInTop = swapRows topRow pivotRow $ rowOpsMatrix |.| xss
              in reduceFromCol (col + 1, topRow + 1) pivotInTop -- Next column.

nullSpaceBasis :: (Fractional a, Eq a) => Matrix a -> [Vector a]
nullSpaceBasis = makeBasis . reducedEchelonForm
  where
    makeBasis :: (Num a, Eq a) => Matrix a -> [Vector a]
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

addColumn :: Matrix a -> Vector a -> Matrix a
addColumn ms v = [row ++ [x] | (row, x) <- zipPedantic ms v]

-- Given a list of vectors, check whether the given vector is in their span.
inSpan :: (Fractional a, Eq a) => [Vector a] -> Vector a -> Bool
inSpan [] b = isZero b
inSpan vs b =
  let as = transpose vs -- Consider vectors of span as columns in a matrix.
      ms = addColumn as b
  in rank as == rank ms
