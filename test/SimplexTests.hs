module SimplexTests (simplexTests) where

import Simplex
import Data.Ratio ((%))

import Test.Tasty
import Test.Tasty.HUnit

simplexTests :: TestTree
simplexTests = testGroup "simplexTests" [
    optimizeTests
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
