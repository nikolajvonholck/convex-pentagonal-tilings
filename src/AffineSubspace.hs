module AffineSubspace (AffineSubspace(..), HyperPlane(..), dimension, space, coordsInSpace, intersectWithHyperPlane, fromRationalAffineSubspace) where

import Vector (Vector, (|+|), (|-|), (|*|), dot, zero)
import Matrix (Matrix, squareMatrix, reducedEchelonForm)
import AlgebraicNumber
import Utils (extract, delta, zipWithPedantic)
import JSON

import Data.List (find, genericLength)
import Data.Maybe (fromJust)

-- An (n-1)-dimensional hyper plane (HP t q) defined by: x * t = q.
data HyperPlane a = HP (Vector a) a deriving (Eq, Show)

-- An affine subspace (ASS p bs hps) defined by: p + span bs.
-- We maintain the invariant that the vectors in bs are linearly independent so
-- that they constitute a basis for the affine subspace.
-- The list hps holds all of the hyperplanes being intersected.
data AffineSubspace a = ASS (Vector a) [Vector a] [HyperPlane a] deriving (Eq, Show)

dimension :: AffineSubspace a -> Integer
dimension (ASS _ bs _) = genericLength bs

-- Returns F^n as an affine subspace of itself.
space :: Num a => Integer -> AffineSubspace a
space n = ASS (zero n) (squareMatrix n delta) [] -- Is in normal form

-- Converts coordinates with respect to the basis of the affine subspace into a
-- point in the surrounding space.
coordsInSpace :: (Num a) => AffineSubspace a -> Vector a -> Vector a
coordsInSpace (ASS p bs _) x =
  let components = zipWithPedantic (|*|) x bs
  in foldl (|+|) p components

-- There are three possible scenarios:
-- 1) The affine subspace is parallel with the hyper plane. Then either:
--    a) The affine subspace is contained in the hyper plane.
--    b) The affine subspace is disjoint with the hyper plane.
-- 2) The affine subspace intersects the hyper plane in an affine subspace of
--    dimension decremented by one.
intersectWithHyperPlane :: (Fractional a, Eq a) => AffineSubspace a -> HyperPlane a -> Maybe (AffineSubspace a)
intersectWithHyperPlane (ass@(ASS p bs hps)) (hp@(HP t q)) =
  let hps' = hp:hps in
  case extract (\b -> t `dot` b /= 0) bs of
    -- The affine subspace is parallel to hyper plane.
    Nothing -> if p `dot` t == q then Just ass else Nothing
    -- The affine subspace intersects the hyper plane.
    Just (vs', u, vs'') ->
      let r = t `dot` u -- The value r is non-zero.
          p' = p |+| (((q - p `dot` t) / r) |*| u)
          bs' = [v |-| ((t `dot` v / r) |*| u) | v <- vs' ++ vs'']
      in return $ normalize p' bs' hps'

-- Assumes matrix bs of basis vectors to have full rank. This is reasonable.
normalize :: (Fractional a, Eq a) => Vector a -> Matrix a -> [HyperPlane a] -> AffineSubspace a
normalize p bs hps =
  let bs' = reducedEchelonForm bs
      p' = foldl (\acc b' ->
          fromJust $ do
            (1, y) <- find ((/=0) . fst) (zip b' acc)
            return $ acc |-| (y |*| b')
        ) p bs' -- Assumes all rows are pivot rows, i.e. full rank.
  in ASS p' bs' hps

fromRationalAffineSubspace :: AffineSubspace Rational -> AffineSubspace AlgebraicNumber
fromRationalAffineSubspace (ASS p bs hps) =
  let p' = map fromRational p
      bs' = map (map fromRational) bs
      hps' = [HP (map fromRational t) (fromRational q) | HP t q <- hps]
  in ASS p' bs' hps'

instance JSON a => JSON (HyperPlane a) where
  toJSON (HP t q) =
    jsonObject [
      ("t", toJSON t),
      ("c", toJSON q)
    ]

instance JSON a => JSON (AffineSubspace a) where
  toJSON (ASS p bs hps) =
    jsonObject [
      ("p", toJSON p),
      ("bs", toJSON bs),
      ("hps", toJSON hps)
    ]
