
module Q3 where

import Debug.Trace

{-# ANN map' "HLint: ignore" #-}
-- Q 3.50
map' :: (a -> b) -> [a] -> [b]
map' proc = f
  where f []   = []
        f (y:ys) = proc y : f ys

-- Q 3.51

-- 0 1 2 3 4 5 6 7 8 9 10
x :: [Int]
x = map (\e -> traceShow e e) [0..10]

-- 5
q0 :: Int
q0 = x !! 5

-- 7
q1 :: Int
q1 = x !! 7

