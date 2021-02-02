module Type (Type(..), knownTypes) where

import GoodSet (VertexTypeSet)
import AffineSubspace (HyperPlane(..), intersectWithHyperPlane, space)
import ConvexPolytope (ConvexPolytope, Strictness(..), constraint, boundedConvexPolytope)
import Permutation (cyclicGroup, permute)
import Utils (enumerate)

import qualified Data.Set as Set
import Data.Set (fromList)
import Control.Monad (foldM)
import Data.Maybe (fromJust)

data Type = T String VertexTypeSet (ConvexPolytope Rational) deriving (Show, Eq)

knownTypes :: [Type]
knownTypes = concat $ typeSymmetries <$> types

typeSymmetries :: (String, VertexTypeSet, [HyperPlane Rational]) -> [Type]
typeSymmetries (name, cvts, cs) =
  [ T name' cvts' (makeLP cs') | (i, r) <- enumerate (cyclicGroup 5),
                                 (j, (mA, mS)) <- enumerate [(id, id), (reflectAngles, reflectSides)],
                                 let name' = name ++ ", rotation " ++ show i ++ if j == 2 then " (reflected)" else "",
                                 let cvts' = Set.map ((permute r) . mA) cvts,
                                 let cs' = [HP (permute r $ mS t) q | HP t q <- cs]]
  where
    reflectSides :: [a] -> [a]
    reflectSides (a:otherSides) = a:(reverse otherSides)
    reflectSides _ = error "Polygon not supported."
    reflectAngles :: [a] -> [a]
    reflectAngles = reverse

makeLP :: [HyperPlane Rational] -> ConvexPolytope Rational
makeLP hps = fromJust $ do
  ass <- foldM intersectWithHyperPlane (space 5) $ (HP [1, 1, 1, 1, 1] 1):hps -- TODO: Perhaps enforce constraints after constructing bounded cp?
  boundedConvexPolytope Strict ass [
      constraint [-1, 0, 0, 0, 0] 0, constraint [1, 0, 0, 0, 0] 1,
      constraint [0, -1, 0, 0, 0] 0, constraint [0, 1, 0, 0, 0] 1,
      constraint [0, 0, -1, 0, 0] 0, constraint [0, 0, 1, 0, 0] 1,
      constraint [0, 0, 0, -1, 0] 0, constraint [0, 0, 0, 1, 0] 1,
      constraint [0, 0, 0, 0, -1] 0, constraint [0, 0, 0, 0, 1] 1
    ] -- (0, 1)^5

-- name, corrected vector types, linear constraints.
types :: [(String, VertexTypeSet, [HyperPlane Rational])]
types = [
    ("Type 1",
      fromList [
        [1, 1, 1, 0, 0] -- A + B + C = 2π
      ],
      [] -- No linear constraints on sides.
    ),
    ("Type 2",
      fromList [
        [1, 1, 0, 1, 0] -- A + B + D = 2π
      ],
      [
        HP [1, 0, 0, -1, 0] 0 -- a = d
      ]
    ),
    ("Type 3",
      fromList [
        [3, 0, 0, 0, 0], -- A = 2π/3
        [0, 0, 3, 0, 0], -- C = 2π/3
        [0, 0, 0, 3, 0]  -- D = 2π/3
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 0, -1, 1, -1] 0 -- d = c + e
      ]
    ),
    ("Type 4",
      fromList [
        [4, 0, 0, 0, 0], -- A = π/2
        [0, 0, 4, 0, 0]  -- C = π/2
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 5",
      fromList [
        [6, 0, 0, 0, 0], -- A = π/3
        [0, 0, 3, 0, 0]  -- C = 2π/3
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 6",
      fromList [
        [1, 1, 0, 1, 0], -- C + E = π => A + B + D = 2π
        [1, 0, 0, 0, 2], -- A = 2C    => A + 2E = 2π
        [0, 1, 2, 1, 0]  -- 2C + D + B = 2π TODO: from wiki figure, check...
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, 0, 0, -1] 0, -- b = e
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 7",
      fromList [
        [0, 2, 1, 0, 0], -- 2B + C = 2π
        [1, 0, 0, 2, 0]  -- A + 2D = 2π
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 8",
      fromList [
        [2, 1, 0, 0, 0], -- 2A + B = 2π
        [0, 0, 1, 2, 0]  -- C + 2D = 2π
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 9",
      fromList [
        [0, 1, 0, 0, 2], -- B + 2E = 2π
        [0, 0, 1, 2, 0]  -- C + 2D = 2π
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 10",
      fromList [
        [0, 0, 0, 0, 4], -- E = π/2
        [2, 0, 0, 2, 0], -- A + D = π
        [1, 2, 0, 0, 0], -- 2B - D = π and A + D = π => A + 2B = 2π
        [0, 0, 2, 1, 0]  -- 2C + D = 2π
      ],
      [
        HP [1, 0, 0, 0, -1] 0, -- a = e
        HP [0, 1, 0, 1, -1] 0 -- b + d = e
      ]
    ),
    ("Type 11",
      fromList [
        [4, 0, 0, 0, 0], -- A = π/2
        [0, 0, 2, 0, 2], -- C + E = π
        [0, 2, 1, 0, 0]  -- 2B + C = 2π
      ],
      [
        HP [0, 0, 0, 1, -1] 0, -- d = e
        HP [-2, 0, -1, 0, 1] 0 -- e = 2a + c
      ]
    ),
    ("Type 12",
      fromList [
        [4, 0, 0, 0, 0], -- A = π/2
        [0, 0, 2, 0, 2], -- C + E = π
        [0, 2, 1, 0, 0]  -- 2B + C = 2π
      ],
      [
        HP [2, 0, -1, 0, -1] 0, -- 2a = c + e
        HP [0, 0, 1, -1, 1] 0   -- c + e = d
      ]
    ),
    ("Type 13",
      fromList [
        [4, 0, 0, 0, 0], -- A = π/2
        [0, 0, 4, 0, 0], -- C = π/2
        [0, 2, 0, 1, 0], -- B = π - D/2 => 2B + D = 2π
        [0, 0, 0, 1, 2]  -- E = π - D/2 => D + 2E = 2π
      ],
      [
        HP [0, 0, 1, -1, 0] 0, -- c = d
        HP [0, 0, 2, 0, -1] 0  -- 2c = e
      ]
    ),
    ("Type 14", -- Mathematics Magazine, 58 (5): p. 308
      fromList [
        [4, 0, 0, 0, 0], -- A = π/2
        [0, 0, 2, 0, 2], -- C + E = π
        [0, 2, 1, 0, 0]  -- 2B + C = 2π
      ],
      [
        HP [0, 0, 0, 1, -1] 0, -- d = e
        HP [-2, 0, 0, 0, 1] 0, -- e = 2a
        HP [1, 0, -1, 0, 0] 0  -- 2a = a + c => a = c
      ]
    ),
    ("Type 15",
      fromList [
        [0, 0, 0, 4, 0], -- 2D = π
        [0, 2, 0, 1, 0], -- 2B + D = 2π
        [0, 0, 2, 0, 1], -- 2C + E = 2π
        [1, 0, 0, 0, 2]  -- A + 2E = 2π
      ],
      [
        HP [1, -2, 0, 0, 0] 0, -- a = 2b
        HP [0, 1, 0, -1, 0] 0, -- b = d
        HP [0, 0, 0, 1, -1] 0  -- d = e
      ]
    ),
    -- Special cases of known types.
    ("Type 16", -- TODO: Check is of type 10
      fromList [
        [0, 1, 1, 0, 1], -- B + C + E = 2π
        [0, 2, 0, 1, 0], -- 2B + D = 2π
        [1, 0, 2, 0, 0]  -- A + 2C = 2π
      ],
      [
        HP [0, 1, 0, -1, 0] 0, -- b = d
        HP [0, 2, 0, 0, -1] 0, -- 2b = e
        HP [1, 0, 0, 0, -1] 0  -- a = e
      ]
    ),
    ("Type 17", -- TODO: Check is of type 2
      fromList [
        [0, 0, 1, 0, 2], -- C + 2E = 2π
        [0, 2, 0, 1, 0]  -- 2B + D = 2π
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0, -- c = d
        HP [0, 0, 0, 1, -1] 0  -- d = e
      ]
    ),
    ("Type 18", -- TODO: Check is of type 2
      fromList [
        [0, 0, 0, 1, 2], -- D + 2E = 2π
        [0, 0, 1, 0, 2], -- C + 2E = 2π
        [0, 1, 0, 1, 1]  -- B + D + E = 2π
      ],
      [
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [1, 0, 0, 0, -1] 0  -- a = e
      ]
    ),
    ("Type 19", -- TODO: Check is of type 1
      fromList [
        [0, 0, 1, 0, 2], -- C + 2E = 2π
        [0, 1, 0, 2, 0]  -- B + 2D = 2π
      ],
      [
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0, -- c = d
        HP [0, 0, 0, 1, -1] 0  -- d = e
      ]
    ),
    -- Degenerate types.
    ("Type 20", -- TODO: Check is degenerate.
      fromList [
        [1, 1, 0, 1, 0] -- A + B + D = 2π
      ],
      [
        HP [1, 0, -1, 0, 0] 0, -- a = c
        HP [0, 1, 0, -1, -1] 0 -- b = d + e

      ]
    ),
    ("Type 21", -- TODO: Check is degenerate.
      fromList [
        [0, 0, 0, 1, 2], -- D + 2E = 2π
        [2, 1, 0, 0, 0]  -- 2A + B = 2π
      ],
      [
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 0, 1, -1] 0  -- d = e
      ]
    ),
    ("Type 22", -- TODO: Check is degenerate.
      fromList [
        [0, 0, 1, 0, 2], -- C + 2E = 2π
        [1, 0, 0, 2, 0]  -- A + 2D = 2π
      ],
      [
        HP [1, -1, 0, 0, 0] 0, -- a = b
        HP [0, 1, -1, 0, 0] 0, -- b = c
        HP [0, 0, 1, -1, 0] 0  -- c = d
      ]
    ),
    ("Type 23", -- TODO: Check is degenerate.
      fromList [
        [0, 2, 0, 1, 0], -- 2B + D = 2π
        [1, 1, 0, 1, 0], -- A + B + D = 2π
        [0, 0, 0, 0, 4]  -- 2E = π
      ],
      [
        HP [0, 1, 0, -2, 0] 0, -- b = 2d
        HP [0, 0, 0, 1, -1] 0  -- d = e
      ]
    ),
    ("Type 24", -- TODO: Check is degenerate.
      fromList [
        [0, 0, 2, 1, 0], -- 2C + D = 2π
        [0, 1, 1, 0, 1], -- B + C + E = 2π
        [1, 2, 0, 0, 0]  -- A + 2B = 2π
      ],
      [
        HP [2, -1, 0, -1, 0] 0, -- 2a = b + d
        HP [0, 1, 0, 1, -2] 0   -- b + d = 2e
      ]
    )
  ]
