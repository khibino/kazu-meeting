--
-- $Header$
--

module Q2_44 (module Q2_44) where

import Painter

inlineUpSplit :: (Integral int, Painter p) => p -> int -> p
inlineUpSplit painter = f
  where f 0 = painter
        f n = below painter $ beside smaller smaller
          where smaller = f (n - 1)
