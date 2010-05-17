module Q2_36 (module Q2_36) where

import Q2_35

accumulateListN :: (a -> b -> b) -> b -> [[a]] -> [b]
accumulateListN op ini = f
  where f ([]:_) = []
        f xs     =
          (accumulateList  op ini $ map head xs) :
          (f $ map tail xs)
          --(accumulateListN op ini $ map tail xs)
