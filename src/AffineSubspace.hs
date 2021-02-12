module AffineSubspace (AffineSubspace(..), Hyperplane(..), dimension, space, coordsInSpace, intersectWithHyperplane, fromRationalAffineSubspace, subset) where

import Vector (Vector, (|+|), (|-|), (|*|), dot, zero)
import Matrix (squareMatrix, isInSpan)
import AlgebraicNumber
import Utils (extract, delta, zipWithPedantic)
import JSON

import Data.List (genericLength)

-- An (n-1)-dimensional hyperplane (HP t q) defined by: x * t = q.
data Hyperplane a = HP (Vector a) a deriving (Eq, Show)

-- An affine subspace (ASS p bs hps) defined by: p + span bs.
-- We maintain the invariant that the vectors in bs are linearly independent so
-- that they constitute a basis for the affine subspace.
-- The list hps holds all of the hyperplanes being intersected.
data AffineSubspace a = ASS (Vector a) [Vector a] [Hyperplane a] deriving (Show)

dimension :: AffineSubspace a -> Integer
dimension (ASS _ bs _) = genericLength bs

-- Returns F^n as an affine subspace of itself.
space :: Num a => Integer -> AffineSubspace a
space n = ASS (zero n) (squareMatrix n delta) []

-- Decides whether the first affine subspace is contained in the second one.
subset :: (Fractional a, Eq a) => AffineSubspace a -> AffineSubspace a -> Bool
ASS p vs _ `subset` ASS q ws _ = p |-| q `isInSpan` ws && all (`isInSpan` ws) vs

-- Converts coordinates with respect to the basis of the affine subspace into a
-- point in the surrounding space.
coordsInSpace :: (Num a) => AffineSubspace a -> Vector a -> Vector a
coordsInSpace (ASS p bs _) x =
  let terms = zipWithPedantic (|*|) x bs
  in foldl (|+|) p terms

-- There are three possible scenarios:
-- 1) The affine subspace is parallel with the hyperplane. Then either:
--    a) The affine subspace is contained in the hyperplane.
--    b) The affine subspace is disjoint with the hyperplane.
-- 2) The affine subspace intersects the hyperplane in an affine subspace of
--    dimension decremented by one.
intersectWithHyperplane :: (Fractional a, Eq a) => AffineSubspace a -> Hyperplane a -> Maybe (AffineSubspace a)
intersectWithHyperplane (ASS p bs hps) (hp@(HP t q)) =
  let hps' = hp:hps in
  case extract (\b -> t `dot` b /= 0) bs of
    -- The affine subspace is parallel to hyperplane.
    Nothing -> if t `dot` p == q then Just (ASS p bs hps') else Nothing
    -- The affine subspace intersects the hyperplane.
    Just (vs', u, vs'') ->
      let r = t `dot` u -- The value r is non-zero.
          p' = p |-| (((p `dot` t - q) / r) |*| u)
          bs' = [v |-| ((t `dot` v / r) |*| u) | v <- vs' ++ vs'']
      in Just (ASS p' bs' hps')

fromRationalAffineSubspace :: AffineSubspace Rational -> AffineSubspace AlgebraicNumber
fromRationalAffineSubspace (ASS p bs hps) =
  let p' = fromRational <$> p
      bs' = [fromRational <$> b | b <- bs]
      hps' = [HP (fromRational <$> t) (fromRational q) | HP t q <- hps]
  in ASS p' bs' hps'

instance JSON a => JSON (Hyperplane a) where
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
