--
-- $Header$
--

module Set (module Set) where

--import Data.List

elementOfSetP0 :: (Ord t) => t -> [t] -> Bool
elementOfSetP0 x = p
  where p [] = False
        p (e:es) | e == x    = True
                 | x < e     = False
                 | otherwise = p es

adjoinSet :: (Ord t) => t -> [t] -> [t]
adjoinSet x = f
  where f [] = [x]
        f set@(e:es) | e == x = set
                     | x < e  = e : x : es
                     | otherwise = e : f es

intersectionSet :: (Ord t) => [t] -> [t] -> [t]
intersectionSet = f
  where f [] _  = []
        f _  [] = []
        f set1 @ (x:xs) set2 @ (y:ys)
          | x == y = x : f xs ys
          | x <  y = f xs set2
          | x >  y = f set1 ys

unionSet :: (Ord t) => [t] -> [t] -> [t]
unionSet = f
  where f [] ys  = ys
        f xs  [] = xs
        f set1 @ (x:xs) set2 @ (y:ys)
          | x == y = x : f xs ys
          | x <  y = x : f xs set2
          | x >  y = y : f set1 ys
