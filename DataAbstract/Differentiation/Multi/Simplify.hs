module DataAbstract.Differentiation.Multi.Simplify (simplify) where

import DataAbstract.Differentiation.Support.Expr
  ( Expression (Expr, Val),
    Operator,
  )

type Operate a = a -> a -> a

simplifyConst :: (Num a, Eq a) => Operate a -> a -> [Expression a] -> (a, [Expression a])
simplifyConst _ unit [] = (unit, [])
simplifyConst op unit (x : xs) =
  let (sum, rest) = simplifyConst op unit xs
   in case x of
        Val v -> (op sum v, rest)
        _ -> (sum, x : rest)

buildExpr :: Expression a -> [Expression a] -> Operator -> Expression a
buildExpr x [] op = x
buildExpr x xs op = foldl (Expr op) x xs

simplify :: (Num a, Eq a) => Operate a -> a -> Operator -> [Expression a] -> Expression a
simplify _ unit _ [] = Val unit
simplify op unit mark terms =
  let (sum, rest) = simplifyConst op unit terms
   in case rest of
        [] -> Val sum
        (x : xs) ->
          if sum == unit
            then buildExpr x xs mark
            else buildExpr (Val sum) rest mark