module Main where

import AlgebraicNumberTests (algebraicNumberTests)
import ChebyshevPolynomialTests (chebyshevPolynomialTests)
import IntervalTests (intervalTests)
import MatrixTests (matrixTests)
import PolynomialTests (polynomialTests)
import TrigonometryTests (trigonometryTests)

import Test.Tasty

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ 20 * 1000000) tests

tests :: TestTree
tests = testGroup "tests" [
    algebraicNumberTests,
    chebyshevPolynomialTests,
    intervalTests,
    matrixTests,
    polynomialTests,
    trigonometryTests
  ]
