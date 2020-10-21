module Vector (Vector, dimension, zero, unit, (|+|), (|-|), (|*|), dot, isZero) where

import Utils (delta, zipWithPedantic)
import Data.List (genericReplicate, genericLength)

type Vector a = [a]

dimension :: Vector a -> Integer
dimension = genericLength

zero :: Num a => Integer -> Vector a
zero n = genericReplicate n 0

unit :: Num a => Integer -> Integer -> Vector a
unit n i = [delta i k | k <- [1..n]]

(|+|) :: Num a => Vector a -> Vector a -> Vector a
(|+|) = zipWithPedantic (+)

(|-|) :: Num a => Vector a -> Vector a -> Vector a
(|-|) = zipWithPedantic (-)

(|*|) :: Num a => a -> Vector a -> Vector a
(|*|) a = map (a*)

dot :: Num a => Vector a -> Vector a -> a
dot vs ws = sum $ zipWithPedantic (*) vs ws

isZero :: (Num a, Eq a) => Vector a -> Bool
isZero = all (==0)
