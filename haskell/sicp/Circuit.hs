{-# LANGUAGE Arrows #-}

module Circuit where

import Data.List
import Prelude hiding((.), id)
import Control.Category as C
import Control.Arrow    as A

newtype Auto i o = A (i -> (o, Auto i o))

instance Category Auto where
  id        = A (\b -> (b, id))
  A g . A f = A (\b -> let (c, f') = f b
                           (d, g') = g c
                       in (d, f' >>> g'))

instance Arrow Auto where
  arr f       = A (\b -> (f b, arr f))
  first (A f) = A (\(b, d) -> let (c, f') = f b
                              in ((c, d), first f'))
                
instance ArrowChoice Auto where
  left (A f) = A lf
    where lf (Left b) = let (c, f') = f b
                        in (Left c, left f')
          lf (Right d) = (Right d, left (A f))

instance ArrowLoop Auto where
  loop (A f) = A (\b -> let ( ~(c, d), f') = f (b, d)
                        in (c, loop f'))


class ArrowLoop a => ArrowCircuit a where
  delay :: b -> a b b

counter :: ArrowCircuit a => a Bool Int
counter =  proc reset -> do
             rec output <- id -< if reset then 0 else next
                 next <- delay 0 -< output + 1
             id -< output

instance ArrowCircuit Auto where
--Auto i o= A  (i  -> (o, Auto i o))
  delay b = A (\b' -> (b, delay b'))

runAuto :: Auto b c -> [b] -> [c]
runAuto (A _) []     = []
runAuto (A f) (b:bs) = let (c, f') = f b in (c: runAuto f' bs)

--not' :: ArrowCircuit a => a Bool Bool
--not' =  arr not

--nand :: ArrowCircuit a => a (Bool, Bool) Bool

toAuto :: (i -> o) -> Auto i o
toAuto =  arr

-- not' :: Auto Bool Bool
-- not' =  toAuto not

-- and' :: Auto [Bool] Bool
-- and' =  toAuto and

-- and2 :: Auto (Bool, Bool) Bool
-- and2 =  toAuto $ uncurry (&&)

-- or2 :: Auto (Bool, Bool) Bool
-- or2 =  toAuto $ uncurry (||)

--遅延なし
nand :: Auto (Bool, Bool) Bool
nand =  toAuto $ not . uncurry (&&)

not' :: Auto Bool Bool
not' =  proc input ->
          nand -< (input, input)

and' :: Auto (Bool, Bool) Bool
--and'' =  nand >>> not' --not' . nand
and' =  proc (i0, i1) -> do
          m0 <- nand -< (i0, i1)
          not' -< m0

or' :: Auto (Bool, Bool) Bool
or' =  proc (i0, i1) -> do
         m0 <- not' -< i0
         m1 <- not' -< i1
         nand -< (m0, m1)

xor :: Auto (Bool, Bool) Bool
--xor =  (nand' &&& or') >>> and'
xor =  proc i -> do
         m0 <- nand -< i 
         m1 <- or'  -< i
         and' -< (m0, m1)

hAdder' :: Auto (Bool, Bool) (Bool, Bool)
hAdder' =  and' &&& xor

hAdder :: Auto (Bool, Bool) (Bool, Bool)
--hAdder =  and' &&& xor
hAdder =  
  proc i -> do
    o0 <- xor  -< i
    o1 <- and' -< i
    id -< (o1, o0)

adder :: Auto ((Bool, Bool), Bool) (Bool, Bool)
adder =  
  proc (i, c) -> do
    (m1, m0) <- hAdder -< i
    (m2, o0) <- hAdder -< (m0, c)
    o1 <- or' -< (m1, m2)
    id -< (o1, o0)


-- 遅延あり
off :: Auto Bool Bool
off = delay False

not'' :: Auto Bool Bool
not'' =  proc input ->
           off <<< off <<< not' -< input

and'' :: Auto (Bool, Bool) Bool
and'' =  proc (i0, i1) ->
          off <<< off <<< off <<< and' -< (i0, i1)

or'' :: Auto (Bool, Bool) Bool
or'' =  proc (i0, i1) ->
          off <<< off <<< off <<< off <<< off <<< or' -< (i0, i1)

hAdder'' :: Auto (Bool, Bool) (Bool, Bool)
hAdder'' =
  proc (a, b) -> do
    d <- or''  -< (a, b)
    c <- and'' -< (a, b)
    e <- not'' -< c
    s <- and'' -< (d, e)
    id -< (s, c)


-- Tools for test
tuples2bit :: [(Bool, Bool)]
tuples2bit =  let b = [False,True] in [(x,y) | x <- b, y <- b ]

test2bit  :: Auto (Bool, Bool) c -> [c]
test2bit c = runAuto c tuples2bit

tuples3bit :: [((Bool, Bool), Bool)]
tuples3bit =  let b = [False,True] in [((x,y), z) | x <- b, y <- b, z <- b ]

test3bit  :: Auto ((Bool, Bool), Bool) c -> [c]
test3bit c = runAuto c tuples3bit

sicpInput :: [(Bool, Bool)]
sicpInput =  replicate 8 (True, False) ++ replicate 10 (True, True)

testSicp :: [(Bool, Bool)]
testSicp =  runAuto hAdder'' sicpInput
        
testSicpPick :: IO ()
testSicpPick =  putStr $ unlines [pick 8, pick 11, pick 16]
  where probe n = (n, testSicp !! n)
        pick  n = let m = n - 1 in show (probe m) ++ " --> " ++ show (probe n)

