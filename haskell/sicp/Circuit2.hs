{-# LANGUAGE Arrows #-}

module Circuit2 where

import Prelude hiding((.), id)
import Control.Category ((.), id)
import Control.Arrow (ArrowLoop, arr, (>>>), (<<<), (&&&))
import Control.Arrow.Transformer (lift)
import Control.Arrow.Operations (ArrowCircuit, delay)
import Control.Arrow.Transformer.Automaton (Automaton, runAutomaton)
import qualified Data.Stream as S

counter :: ArrowCircuit a => a Bool Int
counter =  proc reset -> do
             rec output <- id -< if reset then 0 else next
                 next <- delay 0 -< output + 1
             id -< output

toAutomaton :: ArrowLoop a => a i o -> Automaton a i o
toAutomaton =  lift


type Auto i o = Automaton (->) i o

toAuto :: (i -> o) -> Auto i o
toAuto =  toAutomaton

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


runAuto :: Auto b c -> [b] -> [c]
runAuto a i =
  S.toList $ runAutomaton (arr snd >>> a) ((), S.fromList i)

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

