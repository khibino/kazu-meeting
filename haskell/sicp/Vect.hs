--
-- $Header$
--

module Vect (module Vect) where

class Vect v where
  makeVect :: t -> t -> v
  xcorVect :: v -> t
  ycorVect :: v -> t
  addVect  :: v -> v -> v
  subVect  :: v -> v -> v
  scaleVect :: v -> t -> v
