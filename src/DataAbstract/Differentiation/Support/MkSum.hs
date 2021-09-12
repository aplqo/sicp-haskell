module DataAbstract.Differentiation.Support.MkSum where

import DataAbstract.Differentiation.Support.Expr
  ( Expression (Expr, Val),
    Operator (Add),
  )

mkSum :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
mkSum (Val 1) b = b
mkSum b (Val 1) = b
mkSum (Val a) (Val b) = Val (a + b)
mkSum a b = Expr Add a b

(~+) :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
(~+) = mkSum