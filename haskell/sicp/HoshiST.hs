
module HoshiST where

import Control.Monad.State
--import Control.Monad.State.Lazy

adder :: Int -> State Int ()
adder i' = do i <- get
              put (i + i')
              return ()

makeMonitor :: (MonadState s m, Num s) => (a -> b) -> a -> m b
makeMonitor f x =
  do modify (+ 1)
     return $ f x

makeAdder :: Int -> State Int ()
makeAdder i' = do modify (+ i')
                  return ()

value :: State Int Int
value  = get
