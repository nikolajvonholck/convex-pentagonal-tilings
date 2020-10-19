module Vector (Vector, dimension, zero, unit, (|+|), (|-|), (|*|), dot, isZero) where

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
(v:vs) |+| (w:ws) = (v + w) : (vs |+| ws)
[] |+| [] = []
_ |+| _ = error "Vector addition not defined."

(|-|) :: Num a => Vector a -> Vector a -> Vector a
(v:vs) |-| (w:ws) = (v - w) : (vs |-| ws)
[] |-| [] = []
_ |-| _ = error "Vector subtraction not defined."

(|*|) :: Num a => a -> Vector a -> Vector a
a |*| vs = [a * v | v <- vs]

dot :: Num a => Vector a -> Vector a -> a
(v:vs) `dot` (w:ws) = (v * w) + (vs `dot` ws)
[] `dot` [] = 0
_ `dot` _ = error "Scalar product not defined."

isZero :: (Num a, Eq a) => Vector a -> Bool
isZero = all (==0)
