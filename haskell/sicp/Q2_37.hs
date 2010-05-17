module Q2_37 (module Q2_37) where

import Q2_35
import Q2_36

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct v w =
  accumulateList (+) 0 $ zipWith (*) v w

matrixMulVector :: (Num a) => [[a]] -> [a] -> [a]
matrixMulVector m v =
  map (dotProduct v) m

transpose :: [[a1]] -> [[a1]]
transpose =
  accumulateListN (:) []

matrixMulMatrix :: (Num a1) => [[a1]] -> [[a1]] -> [[a1]]
matrixMulMatrix m n =
  map (matrixMulVector cols) m
  where cols = transpose n
