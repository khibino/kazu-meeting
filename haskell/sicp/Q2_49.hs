--
-- $Header$
--

module Q2_49 (module Q2_49) where

import Data.List

import Vect
import Frame
import Segment
import FuncPainter

vectList :: (Fractional num, Vect vec) => [(num, num)] -> [vec num]
vectList = map (uncurry makeVect)

vertexes :: (Fractional num, Vect vec) => [vec num]
vertexes = vectList [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0)]

--zero  = vertexes !! 0
zero  = head vertexes
edge1 = vertexes !! 1
e1e2  = vertexes !! 2
edge2 = vertexes !! 3

zero, edge1, edge2, e1e2 :: (Fractional num, Vect vec) => vec num

rotList :: [a] -> [a]
rotList l = tail l ++ [head l]

vectPairs :: (Fractional num, Vect vec) => [(vec num, vec num)]
vectPairs = zip vertexes $ rotList vertexes

painterA = segments2painter $ map (uncurry makeSegment) vectPairs

painterB = segments2painter [makeSegment zero  e1e2,
                             makeSegment edge1 edge2]

painterC = segments2painter $ zipWith makeSegment meds (rotList meds)
  where meds = map (scaleVect (1 / 2) . uncurry addVect) vectPairs

-- left top
linesLT = vectList [(0.4,  1.0),
                    (0.35, 0.8),
                    (0.4,  0.6),
                    (0.3,  0.6),
                    (0.2,  0.55),
                    (0.0,  0.8)]

-- left bottom
linesLB = vectList [(0.0,  0.6),
                    (0.2,  0.4),
                    (0.3,  0.55),
                    (0.35, 0.5),
                    (0.25, 0.0)]

-- bottom
linesB  = vectList [(0.4, 0.0),
                    (0.5, 0.2),
                    (0.6, 0.0)]

-- right bottom
linesRB = vectList [(0.75, 0.0),
                    (0.65, 0.5),
                    (1.0,  0.2)]

-- right top
linesRT = vectList [(1.0,  0.4),
                    (0.7,  0.6),
                    (0.6,  0.6),
                    (0.65, 0.8),
                    (0.6,  1.0)]
          
linesLT, linesLB, linesB, linesRB, linesRT :: (Fractional num, Vect vec) => [vec num]

painterD = segments2painter
           $ flatsegs [linesLT, linesLB, linesB, linesRB, linesRT]
  where segs vs = zipWith makeSegment (init vs) (tail vs)
        flatsegs =
          foldl' (\res vecs -> res ++ segs vecs) []

painterA, painterB, painterC, painterD :: (Frame f, Fractional num, Vect vec) => f (vec num) -> ()
