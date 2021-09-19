{-# LANGUAGE NoImplicitPrelude #-}

module DataAbstract.Set.Tree
  ( mkSet,
    elementOfSet,
    adjoin,
    union,
    intersect,
    lookup,
    popFirst,
    Set,
  )
where

import qualified DataAbstract.Set.OrderedList as OL
import Prelude hiding (lookup)

data Tree a
  = Null
  | Branch
      { value :: a,
        left :: Tree a,
        right :: Tree a
      }
  deriving (Show)

type Set a = Tree a

mkSet = Null

elementOfSet :: (Ord a) => a -> Set a -> Bool
elementOfSet _ Null = False
elementOfSet a (Branch v l r) =
  case compare a v of
    LT -> elementOfSet a l
    EQ -> True
    GT -> elementOfSet a r

adjoin :: (Ord a) => a -> Set a -> Set a
adjoin v Null = Branch v Null Null
adjoin v b@(Branch c l r) =
  case compare v c of
    LT -> Branch c (adjoin v l) r
    EQ -> b
    GT -> Branch c l (adjoin v r)

treeToList :: Set a -> [a]
treeToList a = copyToList a []
  where
    copyToList :: Tree a -> [a] -> [a]
    copyToList Null l = l
    copyToList (Branch v l r) lst = copyToList l (v : copyToList r lst)

listToTree :: [a] -> Set a
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

intersect :: (Ord a) => Set a -> Set a -> Set a
intersect x y = listToTree $ OL.intersection (treeToList x) (treeToList y)

union :: (Ord a) => Set a -> Set a -> Set a
union x y = listToTree $ OL.union (treeToList x) (treeToList y)

lookup :: (Ord a) => a -> Set a -> Maybe a
lookup _ Null = Nothing
lookup a (Branch v l r) =
  case compare a v of
    LT -> lookup a l
    EQ -> Just v
    GT -> lookup a r

popFirst :: (Ord a) => Set a -> (Maybe a, Set a)
popFirst Null = (Nothing, Null)
popFirst (Branch v Null r) = (Just v, r)
popFirst (Branch v l r) =
  let (f, t) = popFirst l
   in (f, Branch v t r)
