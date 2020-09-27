module Simplex (optimize, Constraint(..), Objective(..), Solution(..)) where

import Data.Map.Strict (fromList)
import qualified Data.Map.Strict as Map
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.List (sortOn)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe)
--import Debug.Trace (traceShow, trace)

data Constraint = [Rational] :<=: Rational
                | [Rational] :>=: Rational
                | [Rational] :==: Rational
                  deriving (Show, Eq)

data Solution = Infeasible
              | Optimal (Rational, [Rational])
              | Unbounded
              deriving (Show, Eq)

data Objective = Maximize [Rational]
                | Minimize [Rational]
                deriving (Show, Eq)

-- (A, b, c)
type StandardForm = ([[Rational]], [Rational], [Rational])

-- B, A, b, c, nu
type SlackForm = ([Int], [[Rational]], [Rational], [Rational], Rational)

optimize :: (Objective, [Constraint]) -> Solution
optimize (obj, constraints) =
  let standardForm = toStdForm (obj, constraints)
  in case feasibleSlackForm standardForm of
    Nothing -> Infeasible
    Just initialSlackForm -> case simplexLoop initialSlackForm of
      Nothing -> Unbounded
      Just finalSlackForm -> extractSolution obj finalSlackForm

toStdForm :: (Objective, [Constraint]) -> StandardForm
toStdForm (Minimize cs, consts) = toStdForm (Maximize (map negate cs), consts)
toStdForm (Maximize cs, consts) = let (ass, bs) = toStdConsts consts in (ass, bs, cs)
  where
    toStdConsts :: [Constraint] -> ([[Rational]], [Rational])
    toStdConsts ((coefs :==: b):as) = toStdConsts $ (coefs :<=: b):(coefs :>=: b):as
    toStdConsts ((coefs :>=: b):as) = toStdConsts $ ((map negate coefs) :<=: negate b):as
    toStdConsts ((coefs :<=: b):as) = let (ass, bs) = toStdConsts as in (coefs:ass, b:bs)
    toStdConsts [] = ([], [])

-- maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
-- maxBy = maximumBy . comparing

minBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
minBy = minimumBy . comparing

-- 1-indexed
(!) :: [a] -> Int -> a
xs ! n = xs !! (n - 1)

-- 1-indexed
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take (i - 1) xs ++ x : (drop i xs)

-- 1-indexed
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex f xs = fmap (+1) $ List.findIndex f xs

-- 1-indexed
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex x xs = fmap (+1) $ List.elemIndex x xs

-- 1-indexed
-- dropAt :: Int -> [a] -> [a]
-- dropAt i xs = concat [take (i - 1) xs, drop i xs]

-- 1-indexed
enumerate :: [a] -> [(Int, a)]
enumerate = zip [1..]

toSlackForm :: StandardForm -> SlackForm
toSlackForm (ass, bs, cs) =
  let m = length bs
      n = length cs
      mZeros = replicate m 0
      asss = map (\(i, row) -> row ++ replaceAt i 1 mZeros) (enumerate ass)
      css = mulRow (-1) cs ++ mZeros
  in ([(n + 1)..(n + m)], asss, bs, css, 0)

mulRow :: Rational -> [Rational] -> [Rational]
mulRow c = map (*c)

addRows :: [Rational] -> [Rational] -> [Rational]
addRows xs ys = map (uncurry (+)) (zip xs ys)

pivot :: SlackForm -> Int -> Int -> SlackForm
pivot (bv, ass, bs, cs, nu) l e =
  let row = l --bv!l    -- row
      column = e --nv!e -- column
      pivotRow = ass!row -- a_lj, j=1..n
      pivotCell = pivotRow!column -- a_le
      bvHat = replaceAt row column bv
      assHat = map (\(i, as) -> if i == row
          then mulRow (1/pivotCell) as
          else addRows as (mulRow (-1 * as!column / pivotCell) pivotRow)
        ) (enumerate ass)
      bsHat = map (\(i, b) -> if i == row
        then b / pivotCell
        else b - ((ass!i)!column) * (bs!row) / pivotCell)
        (enumerate bs)
      csHat = addRows cs (mulRow (-1 * cs!column / pivotCell) pivotRow)
      nuHat = nu - (cs!column / pivotCell) * (bs!row)
  in (bvHat, assHat, bsHat, csHat, nuHat)

selectEntering :: [Rational] -> Maybe Int
selectEntering cs = let (e, ce) = minBy snd (enumerate cs) -- Bland's rule.
  in if ce < 0 then Just e else Nothing

selectLeaving :: [Rational] -> [Rational] -> [Int] -> Maybe Int
selectLeaving bs column bv =
  if all (<=0) column then Nothing
  else
    let positives = filter (\(_, (_, a)) -> a > 0) (enumerate (zip bs column))
        ratios = map (\(i, (b, a)) -> (i, b / a)) positives
        prioritizedRatios = sortOn (\(i, _) -> bv!i) ratios -- Bland's rule.
    in Just $ fst $ minBy snd prioritizedRatios

-- Assumes basic feasible solution.
simplexLoop :: SlackForm -> Maybe SlackForm
simplexLoop (slackForm@(bv, ass, bs, cs, _)) =
  case selectEntering cs of
    Just e -> -- Find next vertex
      let column = map (!e) ass
      in case selectLeaving bs column bv of
        Nothing -> Nothing
        Just l -> let next = pivot slackForm l e
                  in simplexLoop $ next
    Nothing -> -- We are at an optimal vertex.
      Just slackForm

extractSolution :: Objective -> SlackForm -> Solution
extractSolution obj (bv, _, bs, cs, nu) =
  let n = length cs - length bv
      basicValueMap = fromList $ zip bv bs
      point = map (\i -> fromMaybe 0 (Map.lookup i basicValueMap)) [1..n]
      value = case obj of
        Maximize _ -> nu
        Minimize _ -> -nu
  in Optimal (value, point)

feasibleSlackForm :: StandardForm -> Maybe SlackForm
feasibleSlackForm (standardForm@(ass, bs, cs)) =
  let (initialSlackForm@(_, _, _, initCs, _)) = toSlackForm standardForm
      (k, bk) = minBy snd (enumerate bs)
  in if bk >= 0 -- Initial basic solution is feasible.
    then Just initialSlackForm
    else
      let n = length cs
          lpAux = (map (-1:) ass, bs, -1:(replicate n 0))
          slackAux = toSlackForm lpAux
          feasibleSlackAux = pivot slackAux k 1 -- Makes basic solution feasible.
      in case simplexLoop feasibleSlackAux of
        Nothing -> error "Impossible: Unbounded aux lp"
        Just (optimalAuxSlack@(bvh, assh, _, _, nuh)) ->
          if nuh /= 0
            then Nothing
            else -- Ensure x_0 is not basic.
              let auxFinalSlack =
                    case elemIndex 1 bvh of
                      Just i -> -- x_0 is basic
                        let row = assh!i
                            j = fromJust $ findIndex (/=0) (tail row) -- TODO: why exists ???
                        in pivot optimalAuxSlack i (j + 1) -- TODO: Check wrt. p. 888 m
                      Nothing -> optimalAuxSlack
              in Just $ removeXZero initCs auxFinalSlack

removeXZero :: [Rational] -> SlackForm -> SlackForm
removeXZero cs (bv, ass, bs, _, _) = -- Note: x_0 is not basic.
  let bvh = map (subtract 1) bv
      assh = map tail ass
      bsh = bs
      csh = foldl (\obj (i, row) -> addRows obj (mulRow (-1 * cs!(bvh!i)) row)) cs (enumerate assh)
      nuh = sum $ map (\(i, b) -> -1 * cs!(bvh!i) * b) (enumerate bsh)
  in (bvh, assh, bsh, csh, nuh)

-- showSlack :: SlackForm -> String
-- showSlack (bv, ass, bs, cs, nu) =
--   let vals = [show bv, show ass, show bs, show cs, show nu]
--   in unlines $ map (\(s, v) -> s ++ ": " ++ v) (zip ["bv", "ass", "bs", "cs", "nu"] vals)
