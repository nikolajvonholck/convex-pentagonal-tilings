module Simplex (optimize, Constraint(..), Objective(..), Solution(..)) where

import Utils ((!), enumerate, elemIndex, replaceAt, minBy)
import Vector (Vector, zero, unit, (|+|), (|*|))
import Matrix (Matrix)

import qualified Data.Map.Strict as Map
import Data.List (transpose, sortOn, genericLength)
import Data.Maybe (fromMaybe)

data Constraint = [Rational] :<=: Rational
                | [Rational] :>=: Rational
                | [Rational] :==: Rational
                deriving (Show, Eq)

data Solution = Infeasible
              | Optimal (Rational, Vector Rational)
              | Unbounded
              deriving (Show, Eq)

data Objective = Maximize (Vector Rational)
               | Minimize (Vector Rational)
               deriving (Show, Eq)

-- (A, b, c)
type StandardForm = (Matrix Rational, Vector Rational, Vector Rational)

-- B, A, b, c, nu
type SlackForm = ([Integer], Matrix Rational, Vector Rational, Vector Rational, Rational)

optimize :: Objective -> [Constraint] -> Solution
optimize obj constraints =
  let standardForm = toStdForm obj constraints
  in case feasibleSlackForm standardForm of
    Nothing -> Infeasible
    Just initialSlackForm -> case simplexLoop initialSlackForm of
      Nothing -> Unbounded
      Just finalSlackForm -> extractSolution obj finalSlackForm

toStdForm :: Objective -> [Constraint] -> StandardForm
toStdForm (Minimize cs) consts = toStdForm (Maximize ((-1) |*| cs)) consts
toStdForm (Maximize cs) consts = let (ass, bs) = toStdConsts consts in (ass, bs, cs)
  where
    toStdConsts :: [Constraint] -> (Matrix Rational, Vector Rational)
    toStdConsts ((coefs :==: b):as) = toStdConsts $ (coefs :<=: b):(coefs :>=: b):as
    toStdConsts ((coefs :>=: b):as) = toStdConsts $ (((-1) |*| coefs) :<=: negate b):as
    toStdConsts ((coefs :<=: b):as) = let (ass, bs) = toStdConsts as in (coefs:ass, b:bs)
    toStdConsts [] = ([], [])

toSlackForm :: StandardForm -> SlackForm
toSlackForm (ass, bs, cs) =
  let m = genericLength bs
      n = genericLength cs
      ass' = [row ++ unit m i | (i, row) <- enumerate ass]
      cs' = (-1) |*| cs ++ zero m
  in ([n + 1..n + m], ass', bs, cs', 0)

pivot :: SlackForm -> Integer -> Integer -> SlackForm
pivot (bv, ass, bs, cs, nu) l e =
  let row = l
      column = e
      pivotRow = ass!row
      pivotCell = pivotRow!column
      bv' = replaceAt row column bv
      ass' = map (\(i, as) -> if i == row
          then (1 / pivotCell) |*| as
          else as |+| ((-1 * as!column / pivotCell) |*| pivotRow))
        (enumerate ass)
      bs' = map (\(i, b) -> if i == row
        then b / pivotCell
        else b - ((ass!i)!column) * (bs!row) / pivotCell)
        (enumerate bs)
      cs' = cs |+| ((-1 * cs!column / pivotCell) |*| pivotRow)
      nu' = nu - (cs!column / pivotCell) * (bs!row)
  in (bv', ass', bs', cs', nu')

selectEntering :: [Rational] -> Maybe Integer
selectEntering cs = let (e, ce) = minBy snd $ enumerate cs -- Bland's rule.
  in if ce < 0 then Just e else Nothing

selectLeaving :: [Rational] -> [Rational] -> [Integer] -> Maybe Integer
selectLeaving bs column bv =
  if all (<=0) column then Nothing
  else
    let ratios = [(i, b / a) | (i, (b, a)) <- enumerate (zip bs column), a > 0]
        prioritizedRatios = sortOn (\(i, _) -> bv!i) ratios -- Bland's rule.
    in Just $ fst $ minBy snd prioritizedRatios

-- Assumes basic feasible solution.
simplexLoop :: SlackForm -> Maybe SlackForm
simplexLoop (slackForm@(bv, ass, bs, cs, _)) =
  case selectEntering cs of
    Just e -> do -- Find next vertex.
        let column = (transpose ass) ! e
        l <- selectLeaving bs column bv
        simplexLoop $ pivot slackForm l e
    Nothing -> -- We are at an optimal vertex.
      Just slackForm

extractSolution :: Objective -> SlackForm -> Solution
extractSolution obj (bv, _, bs, cs, nu) =
  let n = genericLength cs - genericLength bv
      basicValueMap = Map.fromList $ zip bv bs
      point = [fromMaybe 0 (Map.lookup i basicValueMap) | i <- [1..n]]
      value = case obj of
        Maximize _ -> nu
        Minimize _ -> -nu
  in Optimal (value, point)

feasibleSlackForm :: StandardForm -> Maybe SlackForm
feasibleSlackForm (standardForm@(ass, bs, cs)) =
  let (initialSlackForm@(_, _, _, initCs, _)) = toSlackForm standardForm
      (k, bk) = minBy snd $ enumerate bs
  in if bk >= 0 -- Initial basic solution is feasible.
    then Just initialSlackForm else
      let n = genericLength cs
          auxSlackForm = toSlackForm ([-1:row | row <- ass], bs, -1:zero n)
          auxFeasibleSlackForm = pivot auxSlackForm k 1
      in case simplexLoop auxFeasibleSlackForm of
        Nothing -> error "Impossible: Unbounded auxiliary problem."
        Just (optimalAuxSlack@(bv', _, _, _, nu')) ->
          if nu' /= 0 then Nothing else
            case elemIndex 1 bv' of -- Ensure x_0 is non-basic variable.
              Just _ -> error "Impossible: x_0 is basic variable."
              Nothing -> Just $ removeXZero initCs optimalAuxSlack

removeXZero :: [Rational] -> SlackForm -> SlackForm
removeXZero cs (bv, ass, bs, _, _) = -- Assumption: x_0 is not basic.
  let bv' = [i - 1 | i <- bv]
      ass' = [tail row | row <- ass]
      cs' = foldl (\obj (i, row) -> obj |+| ((-1 * cs!(bv'!i)) |*| row)) cs $ enumerate ass'
      nu' = sum [-1 * cs!(bv'!i) * b | (i, b) <- enumerate bs]
  in (bv', ass', bs, cs', nu')
