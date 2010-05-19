--
-- $Header$
--

module SplitPainter (module SplitPainter) where

class Painter p where
  beside :: p -> p -> p
  below  :: p -> p -> p

data QTree a =
  Beside (QTree a) (QTree a) |
  Below  (QTree a) (QTree a) |
  Wave a
  deriving Show

instance Painter (QTree a) where
  beside = Beside
  below  = Below


inlineRightSplit :: (Integral int, Painter p) => p -> int -> p
inlineRightSplit painter = f
  where f 0 = painter
        f n = beside painter $ below smaller smaller
          where smaller = f (n - 1)
