--
-- $Header$
--

module Frame (module Frame) where

import Vect

class Frame f where
  makeFrame :: (Num a, Vect vec) => vec a -> vec a -> vec a -> f (vec a)
  originFrame :: (Num a, Vect vec) => f (vec a) -> vec a
  edge1Frame  :: (Num a, Vect vec) => f (vec a) -> vec a
  edge2Frame  :: (Num a, Vect vec) => f (vec a) -> vec a

frameCoordMap :: (Frame f, Num num, Vect v) =>
    f (v num) -> v num -> v num
frameCoordMap frame v =
  addVect
  (originFrame frame)
  $ addVect (scaleV xcorVect edge1Frame) (scaleV ycorVect edge2Frame)
  where scaleV getF getE = scaleVect (getF v) $ getE frame
  
data ListF v = ListF [v]

unListF :: ListF v -> [v]
unListF (ListF vl) = vl

instance Frame ListF where
  makeFrame origin edge1 edge2 = ListF [origin, edge1, edge2]
  originFrame = (!! 0) . unListF
  edge1Frame  = (!! 1) . unListF
  edge2Frame =  (!! 2) . unListF


data TupleF v = TupleF (v, (v, v))

unTupleF :: TupleF v -> (v, (v, v))
unTupleF (TupleF t) = t

instance Frame TupleF where
  makeFrame origin edge1 edge2 = TupleF (origin, (edge1, edge2))
  originFrame = fst . unTupleF
  edge1Frame  = fst . snd . unTupleF
  edge2Frame  = snd . snd . unTupleF
