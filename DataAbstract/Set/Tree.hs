module DataAbstract.Set.Tree
  ( mkSet,
    elementOfSet,
    adjoin,
    union,
    intersect,
  )
where

import qualified DataAbstract.Set.OrderedList as OL

data Tree a
  = Null
  | Branch
      { value :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show)

mkSet = Null

elementOfSet :: (Ord a) => a -> Tree a -> Bool
elementOfSet _ Null = False
elementOfSet a (Branch v l r) =
  case compare a v of
    LT -> elementOfSet a l
    EQ -> True
    GT -> elementOfSet a r

adjoin :: (Ord a) => a -> Tree a -> Tree a
adjoin v Null = Branch v Null Null
adjoin v b@(Branch c l r) =
  case compare v c of
    LT -> Branch c (adjoin v l) r
    EQ -> b
    GT -> Branch c l (adjoin v r)

treeToList :: Tree a -> [a]
treeToList a = copyToList a []
  where
    copyToList :: Tree a -> [a] -> [a]
    copyToList Null l = l
    copyToList (Branch v l r) lst = copyToList l (v : copyToList r lst)

listToTree :: [a] -> Tree a
listToTree lst = fst $ partialTree lst (length lst)
  where
    partialTree :: [a] -> Int -> (Tree a, [a])
    partialTree rest 0 = (Null, rest)
    partialTree rest n =
      let (subL, c : xs) = partialTree rest leftSize
       in let (subR, res) = partialTree xs (n - leftSize - 1)
           in (Branch c subL subR, res)
      where
        leftSize = (n -1) `div` 2

intersect :: (Ord a) => Tree a -> Tree a -> Tree a
intersect x y = listToTree $ OL.intersection (treeToList x) (treeToList y)

union :: (Ord a) => Tree a -> Tree a -> Tree a
union x y = listToTree $ OL.union (treeToList x) (treeToList y)
