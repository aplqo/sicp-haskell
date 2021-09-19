module DataAbstract.HuffmanTree
  ( Tree,
    mkTree,
    Symbol,
    encode,
    decode,
    generateTree,
  )
where

import DataAbstract.Set.Tree

type Symbol = String

data Tree
  = Leaf {weight :: Int, symbol :: Symbol}
  | Branch
      { left :: Tree,
        right :: Tree,
        weight :: Int,
        symbols :: [Symbol]
      }
  deriving (Eq, Show)

instance Ord Tree where
  compare (Leaf w1 s1) (Leaf w2 s2) =
    if w1 /= w2
      then compare w1 w2
      else compare s1 s2
  compare Branch {weight = w1, symbols = s1} Branch {weight = w2, symbols = s2} =
    if w1 /= w2
      then compare w1 w2
      else compare s1 s2
  compare a b =
    let ret = compare (weight a) (weight b)
     in if ret == EQ
          then case a of
            Leaf {} -> LT
            _ -> GT
          else ret

getSymbol :: Tree -> [Symbol]
getSymbol Leaf {symbol = s} = [s]
getSymbol Branch {symbols = s} = s

mkTree :: Tree -> Tree -> Tree
mkTree l r =
  Branch
    { left = l,
      right = r,
      weight = weight l + weight r,
      symbols = getSymbol l ++ getSymbol r
    }

decode :: [Int] -> Tree -> [Symbol]
decode bits tree = decodeOne bits tree
  where
    decodeOne :: [Int] -> Tree -> [Symbol]
    decodeOne [] _ = []
    decodeOne (x : xs) Branch {left = l, right = r} =
      let next = if x == 1 then r else l
       in case next of
            Leaf {symbol = s} -> s : decodeOne xs tree
            Branch {} -> decodeOne xs next

mkLeafSet :: [(Symbol, Int)] -> Set Tree
mkLeafSet = foldr (\(sym, freq) t -> adjoin Leaf {symbol = sym, weight = freq} t) mkSet

encode :: [Symbol] -> Tree -> [Int]
encode syms tree = foldr (doEncode tree) [] syms
  where
    doEncode t sym lst =
      if sym `elem` getSymbol t
        then encodeOne t sym lst
        else error "Not found symbol"

    encodeOne Branch {left = l, right = r} sym lst =
      if sym `elem` getSymbol l
        then 0 : encodeOne l sym lst
        else 1 : encodeOne r sym lst
    encodeOne Leaf {} _ lst = lst

successiveMerge :: Set Tree -> Tree
successiveMerge s =
  let (f, t1) = popFirst s
   in let (s, t2) = popFirst t1
       in case (f, s) of
            (Just a, Just b) -> successiveMerge (adjoin (mkTree a b) t2)
            (Just a, Nothing) -> a

generateTree :: [(Symbol, Int)] -> Tree
generateTree = successiveMerge . mkLeafSet
