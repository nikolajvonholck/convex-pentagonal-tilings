module JSON where

import TilingGraph (InteriorAngle(..), ExteriorAngle(..), Angle(..), Side(..), VertexWithLocation(..), VertexInfo(..))

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, fromList, toAscList, mapKeys)
import Data.List (intercalate)

class JSON a where
  toJSON :: a -> String

instance JSON Double where
  toJSON = show

instance JSON Integer where
  toJSON = show

jsonObject :: Map String String -> String
jsonObject m =
  let entries = toAscList m
      inner = intercalate ", " [show k ++ ": " ++ v | (k, v) <- entries]
  in  "{" ++ inner ++ "}"

instance JSON VertexWithLocation where
  toJSON (VWL x y is) =
    jsonObject $ fromList [
      ("x", toJSON x),
      ("y", toJSON y),
      ("edges", toJSON is)
    ]

instance JSON VertexInfo where
  toJSON (VertexInfo a v s) =
    jsonObject $ fromList [
      ("a", toJSON a),
      ("v", show $ toJSON v),
      ("s", show $ toJSON s)
    ]

instance JSON a => JSON [a] where
  toJSON xs = "[" ++ (intercalate ", " [toJSON x | x <- xs]) ++ "]"

instance (Show a, JSON b) => JSON (Map a b) where
  toJSON m = jsonObject $ mapKeys (show) $ Map.map toJSON m

instance JSON Angle where
  toJSON (Int a) = show $ toJSON a
  toJSON (Ext a) = show $ toJSON a

instance JSON InteriorAngle where
  toJSON AOne = "1"
  toJSON ATwo = "2"
  toJSON AThree = "3"
  toJSON AFour = "4"
  toJSON AFive = "5"

instance JSON ExteriorAngle where
  toJSON Unknown = "?"
  toJSON Zero = "0"
  toJSON Pi = "pi"

instance JSON Side where
  toJSON EOne = "1"
  toJSON ETwo = "2"
  toJSON EThree = "3"
  toJSON EFour = "4"
  toJSON EFive = "5"
