module BinTree (module BinTree) where

data Tree a =
  Leaf |
  Node (Tree a) a (Tree a)
  deriving Show

treeToList1 :: Tree a -> [a]
treeToList1  = f
  where f Leaf = []
        f (Node lb e rb) = f lb ++ e : f rb

treeToList2 :: Tree a -> [a]
treeToList2 = f []
  where f rv Leaf           = rv
        f rv (Node lb e rb) = e : f rv rb `f` lb

partialTree :: [a] -> Int -> (Tree a, [a])
partialTree = flip f
  where f 0 elts = (,) Leaf elts
        f n elts =
          let lsize = quot (n - 1) 2
              (left, this : rest) = f lsize elts
              (right, remaining)  = f (n - lsize - 1) rest
          in (Node left this right, remaining)

