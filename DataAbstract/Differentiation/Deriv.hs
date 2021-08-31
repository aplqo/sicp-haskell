module Differentiation.Deriv where

import DataAbstract.Differentiation.Exponent
import DataAbstract.Differentiation.Support.Expr
import DataAbstract.Differentiation.Support.MkProduct
import DataAbstract.Differentiation.Support.MkSum

deriv :: (Num a, Eq a) => Expression a -> Variable -> Expression a
deriv (Val _) _ = Val 0
deriv (Var x) y = if x == y then Val 1 else Var x
deriv (Expr Multiply a b) x = (a ~* deriv b x) ~+ (deriv a x ~* b)
deriv (Expr Add a b) x = deriv a x ~+ deriv b x
deriv (Expr Exponent base e) x =
  e ~* (base ~** (e ~+ Val (negate 1))) ~* deriv base x