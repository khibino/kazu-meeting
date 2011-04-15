{-# LANGUAGE Arrows #-}

module Circuit2 where

import Prelude hiding((.), id)
import Control.Category as C
import Control.Arrow    as A
import Control.Arrow.Transformer
import Control.Arrow.Operations
import Control.Arrow.Transformer.Automaton
--import qualified Data.Stream as S

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

and'' :: Auto (Bool, Bool) Bool
and'' =  nand >>> not' --not' . nand

and' :: Auto (Bool, Bool) Bool
and' =  proc (i0, i1) -> do
          m0 <- nand -< (i0, i1)
          not' -< m0

or' :: Auto (Bool, Bool) Bool
or' =  proc (i0, i1) -> do
         m0 <- not' -< i0
         m1 <- not' -< i1
         nand -< (m0, m1)

xor' :: Auto (Bool, Bool) Bool
xor' =  (nand &&& or') >>> and'

xor :: Auto (Bool, Bool) Bool
xor =  proc i -> do
         m0 <- nand -< i 
         m1 <- or'  -< i
         and' -< (m0, m1)

hAdder' :: Auto (Bool, Bool) (Bool, Bool)
hAdder' =  and' &&& xor

hAdder :: Auto (Bool, Bool) (Bool, Bool)
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

tuples2bit :: [(Bool, Bool)]
tuples2bit =  let b = [False,True] in [(x,y) | x <- b, y <- b ]

--test2bit  :: Auto (Bool, Bool) c -> S.Stream c
--test2bit c = runAutomaton c $ ((), S.fromList tuples2bit)

--runAuto :: (ArrowLoop a, ArrowApply a) =>
--	Automaton (->) b c -> (->) ((), S.Stream b) (S.Stream c)
-- runAuto :: Automaton (->) b c -> (->) ((), S.Stream b) (S.Stream c)
-- runAuto (Automaton f) = runAutomaton (Automaton (\ ((), v) -> f v))

-- test2bit  :: Auto (Bool, Bool) c -> S.Stream c
-- test2bit c = runAuto c $ S.fromList tuples2bit

tuples3bit :: [((Bool, Bool), Bool)]
tuples3bit =  let b = [False,True] in [((x,y), z) | x <- b, y <- b, z <- b ]

-- test3bit  :: Auto ((Bool, Bool), Bool) c -> S.Stream c
-- test3bit c = runAutomaton c $ S.fromList tuples3bit
