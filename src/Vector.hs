module Vector (Vector, dimension, zero, unit, (|+|), (|-|), (|*|), dot) where

import Utils (delta)
import Data.List (genericReplicate, genericLength)

type Vector a = [a]

dimension :: Vector a -> Integer
dimension = genericLength

zero :: Num a => Integer -> Vector a
zero n = genericReplicate n 0

unit :: Num a => Integer -> Integer -> Vector a
unit n i = [delta i k | k <- [1..n]]

(|+|) :: Num a => Vector a -> Vector a -> Vector a
(|+|) = zipWith (+)

(|-|) :: Num a => Vector a -> Vector a -> Vector a
(|-|) = zipWith (-)

(|*|) :: Num a => a -> Vector a -> Vector a
a |*| vs = [a * v | v <- vs]

dot :: Num a => Vector a -> Vector a -> a
dot v w = sum $ zipWith (*) v w
