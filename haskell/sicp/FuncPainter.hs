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

type FPainter f vec num = f (vec num) -> ()
data FuncP f vec num = FuncP (FPainter f vec num)

transformPainter :: (Frame f, Num num, Vect vec) =>
                    (FPainter f vec num) -> vec num -> vec num -> vec num -> FPainter f vec num
transformPainter painter origin corner1 corner2 frame = v
  where v = painter (makeFrame
                     newOrigin
                     (subVect (m corner1) newOrigin)
                     (subVect (m corner2) newOrigin))
        m = frameCoordMap frame
        newOrigin = m origin
        

instance (Frame f, Fractional num, Vect vec) => Painter (FuncP f vec num) where
  
  beside (FuncP painter1) (FuncP painter2) = FuncP f
    where splitPoint = (makeVect 0.5 0.0)
          paintLeft  = transformPainter painter1 (makeVect 0.0 0.0) splitPoint (makeVect 0.0 1.0)
          paintRight = transformPainter painter2 splitPoint (makeVect 1.0 0.0) (makeVect 0.5 1.0)
          f frame = foldl' (\() () -> ()) () [paintLeft frame, paintRight frame]
  
  below (FuncP painter1) (FuncP painter2) = FuncP f
    where splitPoint = (makeVect 0.0 0.5)
          paintBottom = transformPainter painter1 (makeVect 0.0 0.0) splitPoint (makeVect 1.0 0.0)
          paintTop    = transformPainter painter2 splitPoint (makeVect 0.0 1.0) (makeVect 1.0 0.5)
          f frame = foldl' (\() () -> ()) () [paintBottom frame, paintTop frame]
