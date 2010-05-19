--
-- $Header$
--

module FuncPainter (module FuncPainter) where

import Data.List

import Vect
import Frame
import Segment
import Painter

drawLine :: (Vect vec, Num num) => vec num -> vec num -> ()
drawLine = undefined

segments2painter :: (Frame f, Num num, Vect vec) =>
                    [TupleS (vec num)] -> f (vec num) -> ()
segments2painter segList frame =
  foldl'
  (\_ seg -> drawLine
              (frameMap $ startSegment seg)
              (frameMap $ endSegment seg))
  ()
  segList
    where frameMap = frameCoordMap frame


data FuncP f vec num = FuncP (f (vec num) -> ())

instance (Frame f, Num num, Vect vec) => Painter (FuncP f vec num) where
