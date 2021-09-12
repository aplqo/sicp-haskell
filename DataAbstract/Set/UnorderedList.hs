module DataAbstract.Set.UnorderedList
  ( mkSet,
    elementOfSet,
    adjoin,
    intersection,
    unique,
  )
where

mkSet = []

elementOfSet :: (Eq a) => a -> [a] -> Bool
elementOfSet _ [] = False
elementOfSet a (x : xs) = a == x || elementOfSet a xs

adjoin :: (Eq a) => a -> [a] -> [a]
adjoin x s = if elementOfSet x s then s else x : s

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] _ = []
intersection _ [] = []
intersection (x : xs) s2 =
  if elementOfSet x s2
    then x : intersection xs s2
    else intersection xs s2

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) =
  let rest = unique xs
   in if elementOfSet x rest
        then rest
        else x : rest

union :: (Eq a) => [a] -> [a] -> [a]
union a b = unique (a ++ b)
