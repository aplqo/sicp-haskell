module DataAbstract.Differentiation.Multi where

import DataAbstract.Differentiation.Multi.Simplify
import DataAbstract.Differentiation.Support.Expr

mkSum :: (Num a, Eq a) => [Expression a] -> Expression a
mkSum = simplify (+) 0 Add

mkProduct :: (Num a, Eq a) => [Expression a] -> Expression a
mkProduct = simplify (*) 1 Multiply