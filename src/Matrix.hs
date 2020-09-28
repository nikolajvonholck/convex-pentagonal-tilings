module Matrix (reducedEchelonForm, nullSpaceBasis, Vector, Matrix, dotProduct, zeroVector, unitVector, vectorSum, vectorSubtract, scaleVector, mulRow, addRow) where

import Utils ((!), enumerate, findIndex)
import Data.List (transpose, find, (\\), genericReplicate, genericLength)
import Data.Maybe (catMaybes)

type Vector = [Rational]
type Matrix = [[Rational]]

mRows :: Matrix -> Integer
mRows = genericLength

nCols :: Matrix -> Integer
nCols = genericLength . head

dims :: Matrix -> (Integer, Integer)
dims xss = (mRows xss, nCols xss)

zeroVector :: Integer -> Vector
zeroVector n = genericReplicate n 0

unitVector :: Integer -> Integer -> Vector
unitVector n i = [delta i k | k <- [1..n]]

scaleVector :: Rational -> Vector -> Vector
scaleVector n = map (*n)

vectorSum :: Vector -> Vector -> Vector
vectorSum = zipWith (+)

vectorSubtract :: Vector -> Vector -> Vector
vectorSubtract = zipWith (-)

dotProduct :: Vector -> Vector -> Rational
dotProduct xs ys = if length xs /= length ys
                   then error "dotProduct on different dims"
                   else sum $ zipWith (*) xs ys

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [ map (dotProduct row) (transpose n) | row <- m ]

matrix :: Integer -> Integer -> (Integer -> Integer -> Rational) -> Matrix
matrix m n entry = [[entry i j | j <- [1..n]] | i <- [1..m]]

elementaryMatrix :: Integer -> (Integer -> Integer -> Rational) -> Matrix
elementaryMatrix n = matrix n n

delta :: Integer -> Integer -> Rational
delta x i = if x == i then 1 else 0

swapRows :: Integer -> Integer -> Matrix -> Matrix
swapRows i j xss =
  let m = mRows xss
      elemMatrix = elementaryMatrix m (\r c ->
          if r == i
          then delta j c
          else
            if r == j
            then delta i c-- (if c == i then 1 else 0)
            else delta r c--(if c == r then 1 else 0)
        )
  in matrixProduct elemMatrix xss

mulRow :: Integer -> Rational -> Matrix -> Matrix
mulRow i a xss =
  let m = mRows xss
      elemMatrix = elementaryMatrix m
        (\r c -> if r == i then a * delta r c else delta r c)
  in matrixProduct elemMatrix xss

addRow :: Integer -> Rational -> Integer -> Matrix -> Matrix
addRow i a j xss =
  let m = mRows xss
      elemMatrix = elementaryMatrix m
        (\r c -> if r == i && c == j then a else delta r c)
  in matrixProduct elemMatrix xss

reducedEchelonForm :: Matrix -> Matrix
reducedEchelonForm = reduceFromCol (1, 1)
  where
    reduceFromCol :: (Integer, Integer) -> Matrix -> Matrix
    reduceFromCol (col, topRow) xss =
      let (m, n) = dims xss in
        if col > n || topRow > m then xss -- All cols reduced
        else let column = (transpose xss) ! col
        in case find (\(i, x) -> topRow <= i && x /= 0) (enumerate column) of
            Nothing -> reduceFromCol (col + 1, topRow) xss -- Skip column
            Just (row, pivot) ->
              let elemMatrix = elementaryMatrix m
                    (\r c ->
                        if c == row
                        then if r == row
                          then 1 / pivot
                          else - column!r / pivot
                        else delta r c)
                  reduced = matrixProduct elemMatrix xss
                  pivotAtFirst = swapRows topRow row reduced
              in reduceFromCol (col + 1, topRow + 1) pivotAtFirst

nullSpaceBasis :: Matrix -> [Vector]
nullSpaceBasis = makeBasis . reducedEchelonForm

pivotIndices :: Matrix -> [Integer]
pivotIndices xss =
  catMaybes $ map (\row -> findIndex (/=0) row) xss

makeBasis :: Matrix -> [Vector]
makeBasis xss =
  let n = nCols xss
      pivotIs = pivotIndices xss
      nonPivotIs = [1..n] \\ pivotIs
      indicators = [i `elem` pivotIs | i <- [1..n]]
      negatedXss = map (map negate) xss
      units = map (unitVector n) nonPivotIs
      combined = weave indicators negatedXss units
  in [col | (j, col) <- enumerate (transpose combined), j `elem` nonPivotIs]
  where
    weave [] _ _ = []
    weave (True:_) [] _ = error "Impossible"
    weave (False:_) _ [] = error "Impossible"
    weave (True:xs) (y:ys) zs  = y : weave xs ys zs
    weave (False:xs) ys (z:zs) = z : weave xs ys zs
