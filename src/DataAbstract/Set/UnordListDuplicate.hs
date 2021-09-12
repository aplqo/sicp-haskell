module DataAbstract.Set.UnordListDuplicate where

import DataAbstract.Set.UnorderedList (elementOfSet, intersection, mkSet)

adjoin :: (Eq a) => a -> [a] -> [a]
adjoin x s = x : s

union :: (Eq a) => [a] -> [a] -> [a]
union = (++)

{-
   1. adjoin and union become more effiecient, but elementOfSet become slower
   2. When there many adjoins and unions
   -}
