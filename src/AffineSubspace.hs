module AffineSubspace (AffineSubspace (..), HyperPlane (..), dimension, space, coordsInSpace, intersectWithHyperPlane) where

import Vector (Vector, (|+|), (|-|), (|*|), dot, zero)
import Matrix (Matrix, squareMatrix, reducedEchelonForm)
import Utils (extract, delta)

import Data.List (find, genericLength)
import Data.Maybe (fromJust)

-- An (n-1)-dimensional hyper plane (HP t q) defined by: x * t = q.
data HyperPlane a = HP (Vector a) a deriving (Show)

-- An affine subspace (ASS p bs) defined by: p + span bs.
-- We maintain the invariant that the vectors in bs are linearly independent so
-- that they constitute a basis for the affine subspace.
data AffineSubspace a = ASS (Vector a) [Vector a] deriving (Eq, Show)

dimension :: AffineSubspace a -> Integer
dimension (ASS _ bs) = genericLength bs

-- Returns F^n as an affine subspace of itself.
space :: Num a => Integer -> AffineSubspace a
space n = ASS (zero n) (squareMatrix n delta) -- Is in normal form

-- Converts coordinates with respect to the basis of the affine subspace into a
-- point in the surrounding space.
coordsInSpace :: (Num a) => AffineSubspace a -> Vector a -> Vector a
coordsInSpace (ASS p bs) x =
  let components = [c |*| b | (c, b) <- zip x bs] -- TODO: Assert same length.
  in foldl (|+|) p components

-- There are three possible scenarios:
-- 1) The affine subspace is parallel with the hyper plane. Then either:
--    a) The affine subspace is contained in the hyper plane.
--    b) The affine subspace is disjoint with the hyper plane.
-- 2) The affine subspace intersects the hyper plane in an affine subspace of
--    dimension decremented by one.
intersectWithHyperPlane :: (Fractional a, Eq a) => AffineSubspace a -> HyperPlane a -> Maybe (AffineSubspace a)
intersectWithHyperPlane (ASS p bs) (HP t q) =
  case extract (\b -> t `dot` b /= 0) bs of
    -- The affine subspace is parallel to hyper plane.
    Nothing -> if p `dot` t == q then Just (ASS p bs) else Nothing
    -- The affine subspace intersects the hyper plane.
    Just (vs', (_, u), vs'') ->
      let r = t `dot` u -- The value r is non-zero.
          p' = p |+| (((q - p `dot` t) / r) |*| u)
          bs' = [v |-| ((t `dot` v / r) |*| u) | v <- vs' ++ vs'']
      in return $ normalize p' bs'

-- Assumes matrix bs of basis vectors to have full rank. This is reasonable.
normalize :: (Fractional a, Eq a) => Vector a -> Matrix a -> AffineSubspace a
normalize p bs =
  let bs' = reducedEchelonForm bs
      p' = foldl (\acc b' ->
          fromJust $ do
            (_, y) <- find ((/=0) . fst) (zip b' acc) -- TODO: Assert first coord 'x' in (x, y) is 1.
            return $ acc |-| (y |*| b')
        ) p bs' -- Assumes all rows are pivot rows, i.e. full rank.
  in ASS p' bs'
