--
-- $Header$
--

module Segment (module Segment) where

import Vect

class Segment s where
  makeSegment  :: (Num a, Vect vec) => vec a -> vec a -> s (vec a)
  startSegment :: (Num a, Vect vec) => s (vec a) -> vec a
  endSegment   :: (Num a, Vect vec) => s (vec a) -> vec a


data TupleS v = TupleS (v, v)

unTupleS :: TupleS v -> (v, v)
unTupleS (TupleS t) = t

instance Segment TupleS where
  makeSegment s e = TupleS (s, e)
  startSegment = fst . unTupleS
  endSegment   = snd . unTupleS
