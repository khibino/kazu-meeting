module BinTree (module BinTree) where

import Set as Set

data Tree a =
  Leaf |
  Trunk (Tree a) a (Tree a)
  deriving Show

treeToList1 :: Tree a -> [a]
treeToList1  = f
  where f Leaf = []
        f (Trunk lb e rb) = f lb ++ e : f rb

treeToList2 :: Tree a -> [a]
treeToList2 = f []
  where f rv Leaf           = rv
        f rv (Trunk lb e rb) = e : f rv rb `f` lb

listToTree :: [a] -> Tree a
listToTree l = fst $ partialTree l $ length l

-- 問題 2.64
--   再帰的にリストを分割しながら木を構成
partialTree :: [a] -> Int -> (Tree a, [a])
partialTree = flip f
  where f 0 elts = (,) Leaf elts
        f n elts =
          let lsize = quot (n - 1) 2
              (left, this : rest) = f lsize elts
              (right, remaining)  = f (n - lsize - 1) rest
          in (Trunk left this right, remaining)

-- 問題 2.65
intersectionSet :: Ord a =>
                   Tree a -> Tree a -> Tree a
intersectionSet x y =
  listToTree $ Set.intersectionSet (treeToList2 x) (treeToList2 y)

key :: (k, a) -> k
key  = fst

-- 問題 2.66
lookup :: Ord k => k -> Tree (k, a) -> Maybe (k, a)
lookup gkey tree = f tree
  where f  Leaf = Nothing
        f (Trunk lb e@(k, _) rb) | k == gkey = Just e
                                | k >  gkey = f lb
                                | k <  gkey = f rb
                                | otherwise = undefined
