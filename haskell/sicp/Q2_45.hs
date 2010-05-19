--
-- $Header$
--

module Q2_45 (module Q2_45) where

import SplitPainter

split :: (Integral int, Painter p) =>
         (p -> p -> p) -> (p -> p -> p) -> p -> int -> p
split trp trs =
  f where f painter = rec
            where rec 0 = painter
                  rec n = trp painter $ trs smaller smaller
                    where smaller = rec (n - 1)
  
rightSplit = split beside below
upSplit    = split below beside

rightSplit, upSplit :: (Integral int, Painter p) => p -> int -> p
