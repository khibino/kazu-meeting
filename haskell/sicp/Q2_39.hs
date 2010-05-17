module Q2_39 (module Q2_39) where

import Data.List

reverseR :: [a] -> [a]
reverseR = foldr (\ x y -> y ++ [x]) []

reverseL :: [a] -> [a]
reverseL = foldl' (flip (:)) []
