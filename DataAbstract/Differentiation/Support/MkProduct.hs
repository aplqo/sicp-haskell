module DataAbstract.Differentiation.Support.MkProduct where

import DataAbstract.Differentiation.Support.Expr
  ( Expression (Expr, Val),
    Operator (Multiply),
  )

mkProduct :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
mkProduct (Val 1) b = b
mkProduct b (Val 1) = b
mkProduct (Val a) (Val b) = Val (a * b)
mkProduct a b = Expr Multiply a b

(~*) :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
(~*) = mkProduct