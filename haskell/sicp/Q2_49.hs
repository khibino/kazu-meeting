--
-- $Header$
--

module Q2_49 (module Q2_49) where

import Vect
import Frame
import Segment
import FuncPainter

zero  = makeVect 0.0 0.0
edge1 = makeVect 1.0 0.0
edge2 = makeVect 0.0 1.0
e1e2  = addVect edge1 edge2

zero, edge1, edge2, e1e2 :: (Fractional num, Vect vec) => vec num

vertexList :: (Fractional num, Vect vec) => [vec num]
vertexList = [zero, edge1, e1e2, edge2]

vecPairN :: (Fractional num, Vect vec) => Int -> (vec num, vec num)
vecPairN n = (vc !! n, vc !! (n + 1))
  where vc = cycle vertexList

painterA = segments2painter $ map (seg . vecPairN) [0, 1, 2, 3]
  where seg (s, e) = makeSegment s e

painterB = segments2painter [seg zero  e1e2,
                             seg edge1 edge2]
  where seg = makeSegment
         
painterC = segments2painter $ map seg [0, 1, 2, 3]
  where mid n = let (v1, v2) = vecPairN n in scaleVect (1 / 2) $ addVect v1 v2
        seg n = makeSegment (mid n) $ mid $ (+) n 1

painterD = undefined

painterA, painterB, painterC, painterD :: (Frame f, Fractional num, Vect vec) => f (vec num) -> ()
