module Q3_01 where

import Control.Monad.State

adder   :: (Num a) => a -> State a ()
adder a =  modify (+ a) >> return ()

makeSeed   :: (Num a) => a -> State a ()
makeSeed a =  put a >> return ()

test0 :: (Num a) => a
test0 =  execState (makeSeed 5 >> adder 10 >> adder 10) 0
