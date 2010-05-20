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


zero  = vertexes !! 0
edge1 = vertexes !! 1
e1e2  = vertexes !! 2
edge2 = vertexes !! 3

zero, edge1, edge2, e1e2 :: (Fractional num, Vect vec) => vec num

vecPairs :: (Fractional num, Vect vec) => [(vec num, vec num)]
vecPairs = take 4 $ zip vc $ tail vc
  where vc = cycle vertexes

painterA = segments2painter $ map (uncurry makeSegment) vecPairs

painterB = segments2painter [seg zero  e1e2,
                             seg edge1 edge2]
  where seg = makeSegment
         
painterC = segments2painter $ map seg [0, 1, 2, 3]
  where mid n = scaleVect (1 / 2) $ uncurry addVect $ vecPairs !! n
        seg n = makeSegment (mid n) $ mid $ (+) n 1

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
        flatsegs vss =
          foldl' (\res vecs -> res ++ segs vecs) [] vss

painterA, painterB, painterC, painterD :: (Frame f, Fractional num, Vect vec) => f (vec num) -> ()
