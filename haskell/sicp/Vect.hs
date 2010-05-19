--
-- $Header$
--

module Vect (module Vect) where

class Vect v where
  makeVect :: Num num => num -> num -> v num
  xcorVect :: Num num => v num -> num
  ycorVect :: Num num => v num -> num

--addVect  :: Vect v => v num -> v num -> v
addVect  :: (Vect v, Num num) => v num -> v num -> v num
addVect vA vB =
  makeVect (xcorVect vA + xcorVect vB) (ycorVect vA + ycorVect vB)

subVect  :: (Vect v, Num num) => v num -> v num -> v num
subVect vA vB =
  makeVect (xcorVect vA - xcorVect vB) (ycorVect vA - ycorVect vB)

scaleVect :: (Vect v, Num num) => num -> v num -> v num
scaleVect factor vec =
  makeVect ((xcorVect vec) * factor) (ycorVect vec * factor)
  
data V2 a = V2 a a

instance Vect V2 where
  makeVect = V2
  xcorVect (V2 x _) = x
  ycorVect (V2 _ y) = y
