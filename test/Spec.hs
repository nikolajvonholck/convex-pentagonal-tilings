module Main where

import Simplex
import Matrix
import Data.Ratio ((%))

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "tests" [
    optimizeTests,
    reducedEchelonFormTests,
    nullSpaceBasisTests
  ]

optimizeTests :: TestTree
optimizeTests = testGroup "optimize" [
    testCase "basic feasible" $
      optimize (Maximize [3, 1, 2]) [
          [1, 1, 3] :<=: 30,
          [2, 2, 5] :<=: 24,
          [4, 1, 2] :<=: 36
        ] @?= Optimal (28 % 1, [8 % 1,4 % 1,0 % 1]),
    testCase "basic infeasible" $
      optimize (Maximize [2, -1]) [
          [2, -1] :<=: 2,
          [1, -5] :<=: (-4)
        ] @?= Optimal (2 % 1, [14 % 9, 10 % 9]),
    testCase "1.2" $
      optimize (Maximize [2, 1]) [
          [1, 1] :<=: 4,
          [1, 0] :<=: 2
        ] @?= Optimal (6 % 1, [2 % 1, 2 % 1]),
    testCase "2.1" $
      optimize (Maximize [4, 3, -2]) [
          [1, 1, 2] :<=: 3,
          [1, 2, 1] :<=: 6,
          [1, 0, 0] :<=: 2
        ] @?= Optimal (11 % 1, [2 % 1, 1 % 1, 0 % 1]),
    testCase "2.1b" $
      optimize (Maximize [4, 3]) [
          [1, 1] :<=: 3,
          [1, 2] :<=: 6,
          [1, 0] :<=: 2
        ] @?= Optimal (11 % 1, [2 % 1, 1 % 1]),
    testCase "2.2" $
      optimize (Maximize [4, 3, 6]) [
          [3, 1, 3] :<=: 30,
          [2, 2, 3] :<=: 40
        ] @?= Optimal (70 % 1, [0 % 1, 10 % 1, 20 % 3]),
    testCase "2.4" $
      optimize (Maximize [24, 22, 45]) [
          [2, 1, 3] :<=: 42,
          [2, 1, 2] :<=: 40,
          [1, 1 % 2, 1] :<=: 45
        ] @?= Optimal (882 % 1, [0 % 1, 36 % 1, 2 % 1]),
    testCase "3.a" $
      optimize (Maximize [2, 5]) [
          [3, 2] :>=: 6,
          [2, 1] :<=: 2
        ] @?= Infeasible,
    testCase "3.b" $
      optimize (Maximize [-20, 10, -1]) [
          [3, -1, 5] :<=: 50,
          [1, 0, 1] :<=: 10,
          [1, -1, 4] :<=: 20
        ] @?= Unbounded,
    testCase "3.c" $
      optimize (Maximize [2, 2, 4]) [
          [2, 1, 1] :<=: 2,
          [3, 4, 2] :>=: 8
        ] @?= Optimal (4 % 1, [0 % 1, 2 % 1, 0 % 1]),
    testCase "cyclic" $
      optimize (Maximize [3, 4]) [
          [1, 1] :<=: 5,
          [2, 4] :<=: 12,
          [0, 1] :<=: 3
        ] @?= Optimal (16, [4, 1]),
    testCase "ex 29.3-5" $
      optimize (Maximize [18, 25 % 2]) [
          [1, 1] :<=: 20,
          [1, 0] :<=: 12,
          [0, 1] :<=: 16
        ] @?= Optimal (316, [12, 8]),
    testCase "ex 29.3-6" $
      optimize (Maximize [5, -3]) [
          [1, -1] :<=: 1,
          [2, 1] :<=: 2
        ] @?= Optimal (5, [1, 0]),
    testCase "ex 29.3-7" $
      optimize (Minimize [1, 1, 1]) [
          [2, 15 % 2, 3] :>=: 10000,
          [20, 5, 10] :>=: 30000
        ] @?= Optimal (2250, [1250, 1000, 0]),
    testCase "ex 29.5-5" $
      optimize (Maximize [1, 3]) [
          [1, -1] :<=: 8,
          [-1, -1] :<=: (-3),
          [-1, 4] :<=: 2
        ] @?= Optimal (64 % 3, [34 % 3, 10 % 3]),
    testCase "ex 29.5-6" $
      optimize (Maximize [1, -2]) [
          [1, 2] :<=: 4,
          [-2, -6] :<=: (-12),
          [0, 1] :<=: 1
        ] @?= Infeasible,
    testCase "ex 29.5-7" $
      optimize (Maximize [1, 3]) [
          [-1, 1] :<=: (-1),
          [-1, -1] :<=: (-3),
          [-1, 4] :<=: 2
        ] @?= Unbounded,
    testCase "ex 29.5-8" $
      optimize (Minimize [1, 1, 1, 1]) [
          [-2, 8, 0, 10] :>=: 50,
          [5, 2, 0, 0] :>=: 100,
          [3, -5, 10, -2] :>=: 25
        ] @?= Optimal (3100 % 111,[2050 % 111,425 % 111,0 % 1,625 % 111])
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
