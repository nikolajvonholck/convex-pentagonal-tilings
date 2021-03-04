module JSON where

import Data.Map.Strict (Map, toAscList)
import Data.Set (Set, elems)
import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator)
import AlgebraicNumber (AlgebraicNumber, approximate)

class JSON a where
  toJSON :: a -> String

jsonObject :: [(String, String)] -> String
jsonObject entries =
  "{" ++ (intercalate ", " [show k ++ ": " ++ v | (k, v) <- entries]) ++ "}"

instance JSON Double where
  toJSON = show

instance JSON Integer where
  toJSON = show

instance JSON a => JSON [a] where
  toJSON xs = "[" ++ (intercalate ", " [toJSON x | x <- xs]) ++ "]"

instance JSON a => JSON (Set a) where
  toJSON = toJSON . elems

instance (Show a, JSON b) => JSON (Map a b) where
  toJSON m = jsonObject [(show k, toJSON v) | (k, v) <- toAscList m]

instance JSON a => JSON (Ratio a) where
  toJSON x = jsonObject [
      ("p", toJSON $ numerator x),
      ("q", toJSON $ denominator x)
    ]

instance JSON AlgebraicNumber where
  toJSON x = toJSON $ approximate 0.001 x
