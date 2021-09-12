module DataAbstract.Set.OrderedList where

mkSet = []

elementOfSet :: (Ord a) => a -> [a] -> Bool
elementOfSet _ [] = False
elementOfSet a (x : xs) =
  case compare a x of
    LT -> False
    EQ -> True
    GT -> elementOfSet a xs

adjoin :: (Ord a) => a -> [a] -> [a]
adjoin x [] = [x]
adjoin v s@(x : xs) =
  case compare v x of
    LT -> v : s
    EQ -> s
    GT -> x : adjoin v xs

intersection :: (Ord a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection a@(x : xs) b@(y : ys) =
  case compare x y of
    LT -> intersection xs b
    EQ -> x : intersection xs ys
    GT -> intersection a ys

union :: (Ord a) => [a] -> [a] -> [a]
union [] a = a
union a [] = a
union a@(x : xs) b@(y : ys) =
  case compare x y of
    LT -> x : union xs b
    EQ -> x : union xs ys
    GT -> y : union a ys
