module DataAbstract.Differentiation.Exponent where

import DataAbstract.Differentiation.Support.Expr
  ( Expression (Expr, Val),
    Operator (Exponent),
  )

mkExponent :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
mkExponent _ (Val 0) = Val 1
mkExponent a (Val 1) = a
mkExponent a b = Expr Exponent a b

(~**) :: (Num a, Eq a) => Expression a -> Expression a -> Expression a
(~**) = mkExponent