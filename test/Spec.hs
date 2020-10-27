module Main where

import AlgebraicNumberTests (algebraicNumberTests)
import IntervalTests (intervalTests)
import MatrixTests (matrixTests)
import PolynomialTests (polynomialTests)
import SimplexTests (simplexTests)

import Test.Tasty

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ 20 * 1000000) tests

tests :: TestTree
tests = testGroup "tests" [
    algebraicNumberTests,
    intervalTests,
    matrixTests,
    polynomialTests,
    simplexTests
  ]
