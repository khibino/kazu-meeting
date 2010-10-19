module Q3_02 where

import Control.Monad.State

makeMonitor :: (MonadState s m, Num s) => (a -> b) -> a -> m b
makeMonitor f x = modify (+ 1) >> return (f x)

add3 :: (Num s, MonadState s m, Num a) => a -> m a
add3 =  makeMonitor (+ 3)

runMonitor :: (Num s, Num a) => State s a -> (a, s)
runMonitor = (`runState` 0)

test0 :: (Num s, Num a) => (a, s)
test0 =  runMonitor (add3 10 >>= add3)
