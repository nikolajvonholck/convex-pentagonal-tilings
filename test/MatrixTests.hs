module MatrixTests (matrixTests) where

import Matrix
import Data.Ratio ((%))

import Test.Tasty
import Test.Tasty.HUnit

matrixTests :: TestTree
matrixTests = testGroup "matrixTests" [
    reducedEchelonFormTests,
    nullSpaceBasisTests
  ]

reducedEchelonFormTests :: TestTree
reducedEchelonFormTests = testGroup "reducedEchelonForm" [
    testCase "1x1 unit" $
      reducedEchelonForm [
        [1]]
      @?= ([
        [1]] :: Matrix Rational),
    testCase "1x1 simple" $
      reducedEchelonForm [
        [-10]]
      @?= ([
        [1]] :: Matrix Rational),
    testCase "2x2 unit" $
      reducedEchelonForm [
        [1, 0],
        [0, 1]]
      @?= ([
        [1, 0],
        [0, 1]] :: Matrix Rational),
    testCase "3x3 unit" $
      reducedEchelonForm [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]]
      @?= ([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]] :: Matrix Rational),
    testCase "X.1" $
      reducedEchelonForm [
        [1, 2, -1, 4, 5],
        [-4, 2, 6, 0, -2],
        [1, 3, 0, 4, 10]]
      @?= ([
        [1, 0, 0, -2, 7],
        [0, 1, 0, 2, 1],
        [0, 0, 1, -2, 4]] :: Matrix Rational),
    testCase "X.2" $
      reducedEchelonForm [
        [1, 0, 1],
        [1, 1, 1],
        [3, 0, 2]]
      @?= ([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]] :: Matrix Rational),
    testCase "X.3" $
      reducedEchelonForm [
        [2, 1, 0],
        [1, 1, -1],
        [0, -1, 2],
        [3, 1, 1]]
      @?= ([
        [1, 0, 1],
        [0, 1, -2],
        [0, 0, 0],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.4" $
      reducedEchelonForm [
        [2, 3, 1, 1, 0, 0],
        [1, 1, 1, 0, 1, 0],
        [-1, -2, 1, 0, 0, 1]]
      @?= ([
        [1, 0, 0, -3, 5, -2],
        [0, 1, 0, 2, -3, 1],
        [0, 0, 1, 1, -1, 1]] :: Matrix Rational),
    testCase "X.5" $
      reducedEchelonForm [
        [1, 2, -4, 1, 0],
        [0, 2, 5, -4, 0],
        [2, 5, -7, -3, 0]]
      @?= ([
        [1, 0, 0, 23, 0],
        [0, 1, 0, -7, 0],
        [0, 0, 1, 2, 0]] :: Matrix Rational),
    testCase "X.6" $
      reducedEchelonForm [
        [2, 1, 6, 1, 4],
        [-2, -3, 4, 1, 2]]
      @?= ([
        [1, 0, 11 % 2, 1, 7 % 2],
        [0, 1, -5, -1, -3]] :: Matrix Rational),
    testCase "X.7" $
      reducedEchelonForm [
        [-1, 0, 2, -5, 0],
        [2, -1, -3, 0, 0],
        [1, 0, -2, 4, 1]]
      @?= ([
        [1, 0, -2, 0, 5],
        [0, 1, -1, 0, 10],
        [0, 0, 0, 1, -1]] :: Matrix Rational),
    testCase "X.8" $
      reducedEchelonForm [
        [1, 2, -1, 4, 5],
        [-4, 2, 6, 2, 2],
        [2, 3, 0, 4, 10]]
      @?= ([
        [1, 0, 0, -1, 2],
        [0, 1, 0, 2, 2],
        [0, 0, 1, -1, 1]] :: Matrix Rational),
    testCase "X.9" $
      reducedEchelonForm [
        [1, 0, 2],
        [0, 1, -1],
        [1, 2, 0],
        [1, -1, 3]]
      @?= ([
        [1, 0, 2],
        [0, 1, -1],
        [0, 0, 0],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.10 (i)" $
      reducedEchelonForm [
        [1, 2, 1],
        [3, 1, -2],
        [0, 1, 1]]
      @?= ([
        [1, 0, -1],
        [0, 1, 1],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.11" $
      reducedEchelonForm [
        [1, 0, 0, 0],
        [2, 1, 0, 1],
        [0, 1, 2, 0],
        [1, 1, 1, 0]]
      @?= ([
        [1, 0, 0, 0],
        [0, 1, 0, 0],
        [0, 0, 1, 0],
        [0, 0, 0, 1]] :: Matrix Rational),
    testCase "X.12" $
      reducedEchelonForm [
        [1, -2, 2, 5],
        [-2, 4, 1, 0],
        [3, -6, 4, 11]]
      @?= ([
        [1, -2, 0, 1],
        [0, 0, 1, 2],
        [0, 0, 0, 0]] :: Matrix Rational),
    testCase "X.13" $
      reducedEchelonForm [
        [-1, 0, 1],
        [0, 1, -1],
        [-1, 2, 0],
        [1, -1, 3]]
      @?= ([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.14" $
      reducedEchelonForm [
        [1, -1, 1],
        [2, 1, 5]]
      @?= ([
        [1, 0, 2],
        [0, 1, 1]] :: Matrix Rational),
    testCase "X.15" $
      reducedEchelonForm [
        [0, 1, 2],
        [2, 1, 4],
        [1, 0, 1]]
      @?= ([
        [1, 0, 1],
        [0, 1, 2],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.16" $
      reducedEchelonForm [
        [1, -1],
        [2, 1],
        [1, 0]]
      @?= ([
        [1, 0],
        [0, 1],
        [0, 0]] :: Matrix Rational),
    testCase "X.17" $
      reducedEchelonForm [
        [0, -1, 0, 0, 0],
        [1, 0, 0, 0, 0],
        [0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0],
        [0, 0, 0, -1, 0]]
      @?= ([
        [1, 0, 0, 0, 0],
        [0, 1, 0, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 0, 1, 0],
        [0, 0, 0, 0, 1]] :: Matrix Rational),
    testCase "X.18 (i)" $
      reducedEchelonForm [
        [1, 3, 2],
        [1, 2, 1],
        [1, 1, 0],
        [1, 1, 0]]
      @?= ([
        [1, 0, -1],
        [0, 1, 1],
        [0, 0, 0],
        [0, 0, 0]] :: Matrix Rational),
    testCase "X.18 (ii)" $
      reducedEchelonForm [
        [1, 1, 1, 1],
        [3, 2, 1, 1],
        [2, 1, 0, 0]]
      @?= ([
        [1, 0, -1, -1],
        [0, 1, 2, 2],
        [0, 0, 0, 0]] :: Matrix Rational),
    testCase "X.21" $
      reducedEchelonForm [
        [1, 0, 3]]
      @?= ([
        [1, 0, 3]] :: Matrix Rational),
    testCase "X.23" $
      reducedEchelonForm [
        [1, 0, 0, 0, 0],
        [0, -1, 0, 0, 0],
        [0, 0, 0, 0, 1],
        [0, 0, 0, -1, 0],
        [0, 0, -1, 0, 0]]
      @?= ([
        [1, 0, 0, 0, 0],
        [0, 1, 0, 0, 0],
        [0, 0, 1, 0, 0],
        [0, 0, 0, 1, 0],
        [0, 0, 0, 0, 1]] :: Matrix Rational),
    testCase "X.29 (i)" $
      reducedEchelonForm [
        [-4, -4],
        [8, 8]]
      @?= ([
        [1, 1],
        [0, 0]] :: Matrix Rational),
    testCase "X.29 (ii)" $
      reducedEchelonForm [
        [-8, -4],
        [8, 4]]
      @?= ([
        [1, 1 % 2],
        [0, 0]] :: Matrix Rational),
    testCase "X.30" $
      reducedEchelonForm [
        [2, -1, 1, 0],
        [4, -2, 2, 0],
        [8, -4, 4, 0]]
      @?= ([
        [1, -1 % 2, 1 % 2, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]] :: Matrix Rational)
  ]


nullSpaceBasisTests :: TestTree
nullSpaceBasisTests = testGroup "nullSpaceBasis" [
    testCase "1x1 zero" $
      nullSpaceBasis [
        [0]]
      @?= ([
        [1]] :: Matrix Rational),
    testCase "2x2 zero" $
      nullSpaceBasis [
        [0, 0],
        [0, 0]]
      @?= ([
        [1, 0],
        [0, 1]] :: Matrix Rational),
    testCase "3x3 zero" $
      nullSpaceBasis [
        [0, 0, 0],
        [0, 0, 0],
        [0, 0, 0]]
      @?= ([
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]] :: Matrix Rational),
    testCase "1x1 unit" $
      nullSpaceBasis [
        [1]]
      @?= ([] :: Matrix Rational),
    testCase "1x1 simple" $
      nullSpaceBasis [
        [-10]]
      @?= ([] :: Matrix Rational),
    testCase "2x2 unit" $
      nullSpaceBasis [
        [1, 0],
        [0, 1]]
      @?= ([] :: Matrix Rational),
    testCase "3x3 unit" $
      nullSpaceBasis [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]]
      @?= ([] :: Matrix Rational),
    testCase "special 1" $
      nullSpaceBasis [
        [1, 2, 0, 9, 7, 0, 0],
        [0, 0, 1, 3, 4, 0, 0],
        [0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 1]]
      @?= ([
        [-2, 1, 0, 0, 0, 0, 0],
        [-9, 0, -3, 1, 0, 0, 0],
        [-7, 0, -4, 0, 1, 0, 0]] :: Matrix Rational),
    testCase "special 2" $
      nullSpaceBasis [
        [0, 2, 0, 10, 6, 0, 0],
        [0, 0, 1, 3, 4, 0, 0],
        [0, 0, 0, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0]]
      @?= ([
        [1, 0, 0, 0, 0, 0, 0],
        [0, -5, -3, 1, 0, 0, 0],
        [0, -3, -4, 0, 1, 0, 0],
        [0, 0, 0, 0, 0, 0, 1]] :: Matrix Rational),
    testCase "X.1" $
      nullSpaceBasis [
        [1, 2, -1, 4, 5],
        [-4, 2, 6, 0, -2],
        [1, 3, 0, 4, 10]]
      @?= ([
        [2, -2, 2, 1, 0],
        [-7, -1, -4, 0, 1]] :: Matrix Rational),
    testCase "X.2" $
      nullSpaceBasis [
        [1, 0, 1],
        [1, 1, 1],
        [3, 0, 2]]
      @?= ([] :: Matrix Rational),
    testCase "X.3" $
      nullSpaceBasis [
        [2, 1, 0],
        [1, 1, -1],
        [0, -1, 2],
        [3, 1, 1]]
      @?= ([
        [-1, 2, 1]] :: Matrix Rational),
    testCase "X.4" $
      nullSpaceBasis [
        [2, 3, 1, 1, 0, 0],
        [1, 1, 1, 0, 1, 0],
        [-1, -2, 1, 0, 0, 1]]
      @?= ([
        [3, -2, -1, 1, 0, 0],
        [-5, 3, 1, 0, 1, 0],
        [2, -1, -1, 0, 0, 1]] :: Matrix Rational),
    testCase "X.5" $
      nullSpaceBasis [
        [1, 2, -4, 1, 0],
        [0, 2, 5, -4, 0],
        [2, 5, -7, -3, 0]]
      @?= ([
        [-23, 7, -2, 1, 0],
        [0, 0, 0, 0, 1]] :: Matrix Rational),
    testCase "X.6" $
      nullSpaceBasis [
        [2, 1, 6, 1, 4],
        [-2, -3, 4, 1, 2]]
      @?= ([
        [-11 % 2, 5, 1, 0, 0],
        [-1, 1, 0, 1, 0],
        [-7 % 2, 3, 0, 0, 1]] :: Matrix Rational),
    testCase "X.7" $
      nullSpaceBasis [
        [-1, 0, 2, -5, 0],
        [2, -1, -3, 0, 0],
        [1, 0, -2, 4, 1]]
      @?= ([
        [2, 1, 1, 0, 0],
        [-5, -10, 0, 1, 1]] :: Matrix Rational),
    testCase "X.8" $
      nullSpaceBasis [
        [1, 2, -1, 4, 5],
        [-4, 2, 6, 2, 2],
        [2, 3, 0, 4, 10]]
      @?= ([
        [1, -2, 1, 1, 0],
        [-2, -2, -1, 0, 1]] :: Matrix Rational),
    testCase "X.9" $
      nullSpaceBasis [
        [1, 0, 2],
        [0, 1, -1],
        [1, 2, 0],
        [1, -1, 3]]
      @?= ([
        [-2, 1, 1]] :: Matrix Rational),
    testCase "X.10 (i)" $
      nullSpaceBasis [
        [1, 2, 1],
        [3, 1, -2],
        [0, 1, 1]]
      @?= ([
        [1, -1, 1]] :: Matrix Rational),
    testCase "X.11" $
      nullSpaceBasis [
        [1, 0, 0, 0],
        [2, 1, 0, 1],
        [0, 1, 2, 0],
        [1, 1, 1, 0]]
      @?= ([] :: Matrix Rational),
    testCase "X.12" $
      nullSpaceBasis [
        [1, -2, 2, 5],
        [-2, 4, 1, 0],
        [3, -6, 4, 11]]
      @?= ([
        [2, 1, 0, 0],
        [-1, 0, -2, 1]] :: Matrix Rational),
    testCase "X.13" $
      nullSpaceBasis [
        [-1, 0, 1],
        [0, 1, -1],
        [-1, 2, 0],
        [1, -1, 3]]
      @?= ([] :: Matrix Rational),
    testCase "X.14" $
      nullSpaceBasis [
        [1, -1, 1],
        [2, 1, 5]]
      @?= ([
        [-2, -1, 1]] :: Matrix Rational),
    testCase "X.15" $
      nullSpaceBasis [
        [0, 1, 2],
        [2, 1, 4],
        [1, 0, 1]]
      @?= ([
        [-1, -2, 1]] :: Matrix Rational),
    testCase "X.16" $
      nullSpaceBasis [
        [1, -1],
        [2, 1],
        [1, 0]]
      @?= ([] :: Matrix Rational),
    testCase "X.17" $
      nullSpaceBasis [
        [0, -1, 0, 0, 0],
        [1, 0, 0, 0, 0],
        [0, 0, 0, 0, 1],
        [0, 0, 1, 0, 0],
        [0, 0, 0, -1, 0]]
      @?= ([] :: Matrix Rational),
    testCase "X.18 (i)" $
      nullSpaceBasis [
        [1, 3, 2],
        [1, 2, 1],
        [1, 1, 0],
        [1, 1, 0]]
      @?= ([
        [1, -1, 1]] :: Matrix Rational),
    testCase "X.18 (ii)" $
      nullSpaceBasis [
        [1, 1, 1, 1],
        [3, 2, 1, 1],
        [2, 1, 0, 0]]
      @?= ([
        [1, -2, 1, 0],
        [1, -2, 0, 1]] :: Matrix Rational),
    testCase "X.21" $
      nullSpaceBasis [
        [1, 0, 3]]
      @?= ([
        [0, 1, 0],
        [-3, 0, 1]] :: Matrix Rational),
    testCase "X.23" $
      nullSpaceBasis [
        [1, 0, 0, 0, 0],
        [0, -1, 0, 0, 0],
        [0, 0, 0, 0, 1],
        [0, 0, 0, -1, 0],
        [0, 0, -1, 0, 0]]
      @?= ([] :: Matrix Rational),
    testCase "X.29 (i)" $
      nullSpaceBasis [
        [-4, -4],
        [8, 8]]
      @?= ([
        [-1, 1]] :: Matrix Rational),
    testCase "X.29 (ii)" $
      nullSpaceBasis [
        [-8, -4],
        [8, 4]]
      @?= ([
        [-1 % 2, 1]] :: Matrix Rational),
    testCase "X.30" $
      nullSpaceBasis [
        [2, -1, 1],
        [4, -2, 2],
        [8, -4, 4]]
      @?= ([
        [1 % 2, 1, 0],
        [-1 % 2, 0, 1]] :: Matrix Rational)
  ]
