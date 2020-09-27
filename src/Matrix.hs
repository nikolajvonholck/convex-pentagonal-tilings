module Matrix (reducedEchelonForm, nullSpaceBasis, Vector, Matrix, dotProduct, zeroVector, unitVector, vectorSum, vectorSubtract, scaleVector) where
import Data.List (transpose, find, (\\))
import qualified Data.List as List
import Data.Maybe (catMaybes)
--import Debug.Trace (traceShow, trace)

type Vector = [Rational]
type Matrix = [[Rational]]

mRows :: Matrix -> Int
mRows = length

nCols :: Matrix -> Int
nCols = length . head

dims :: Matrix -> (Int, Int)
dims xss = (mRows xss, nCols xss)

zeroVector :: Int -> Vector
zeroVector n = replicate n 0

unitVector :: Int -> Int -> Vector
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

matrix :: Int -> Int -> (Int -> Int -> Rational) -> Matrix
matrix m n entry = [[entry i j | j <- [1..n]] | i <- [1..m]]

elementaryMatrix :: Int -> (Int -> Int -> Rational) -> Matrix
elementaryMatrix n = matrix n n

delta :: Int -> Int -> Rational
delta x i = if x == i then 1 else 0

swapRows :: Int -> Int -> Matrix -> Matrix
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

mulRow :: Int -> Rational -> Matrix -> Matrix
mulRow i a xss =
  let m = mRows xss
      elemMatrix = elementaryMatrix m
        (\r c -> if r == i then a * delta r c else delta r c)
  in matrixProduct elemMatrix xss

addRow :: Int -> Rational -> Int -> Matrix -> Matrix
addRow i a j xss =
  let m = mRows xss
      elemMatrix = elementaryMatrix m
        (\r c -> if r == i && c == j then a else delta r c)
  in matrixProduct elemMatrix xss

-- 1-indexed
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f xs = fmap (+1) $ List.findIndex f xs

reducedEchelonForm :: Matrix -> Matrix
reducedEchelonForm = reduceFromCol (1, 1)
  where
    reduceFromCol :: (Int, Int) -> Matrix -> Matrix
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

pivotIndices :: Matrix -> [Int]
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

-- 1-indexed
(!) :: [a] -> Int -> a
xs ! n = xs !! (n - 1)

-- 1-indexed
enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]
