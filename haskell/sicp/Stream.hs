
module Stream where

add :: Num a => [a] -> [a] -> [a]
add =  zipWith (+)

mul :: Num a => [a] -> [a] -> [a]
mul =  zipWith (*)

fibs :: Num a => [a]
fibs =  0:1:add (tail fibs) fibs

scale :: Num a => [a] -> a -> [a]
scale =  flip (map . (*))

double :: Num a => [a]
double =  1 : scale double 2

merge :: (Num a, Ord a) => [a] -> [a] -> [a]
merge =  f
  where f []     ys     = ys
        f xs     []     = xs
        f xxs@(x:xs) yys@(y:ys) | x < y     = x : f xs yys
                                | otherwise = y : f xxs ys
