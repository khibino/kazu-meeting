{--# LANGUAGE Arrows #--}

module Constraint where

--import Data.List
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

instance ArrowCircuit Auto where
--Auto i o= A  (i  -> (o, Auto i o))
  delay b = A (\b' -> (b, delay b'))

runAuto :: Auto b c -> [b] -> [c]
runAuto (A _) []     = []
runAuto (A f) (b:bs) = let (c, f') = f b in (c: runAuto f' bs)

toAuto :: (i -> o) -> Auto i o
toAuto =  arr

type BinOp a = a -> a -> a
type TrBinOp a = (BinOp a, BinOp a, BinOp a)
type TrMaybe a = (Maybe a, Maybe a, Maybe a)

op2Constraint :: Fractional a =>
                 TrBinOp a -> Auto (TrMaybe a) (TrMaybe a)
op2Constraint (f, g, h) = toAuto mp
  where mp (Just x, Just y, Nothing) = (Nothing, Nothing, Just (f x y))
        mp (Just x, Nothing, Just z) = (Nothing, Just (g z x), Nothing)
        mp (Nothing, Just y, Just z) = (Just (h z y), Nothing, Nothing)
        mp _                         = error "Invalid state"

plus :: Fractional a => Auto (TrMaybe a) (TrMaybe a)
plus = op2Constraint ((+), (-), (-))

mul :: Fractional a => Auto (TrMaybe a) (TrMaybe a)
mul  = op2Constraint ((*), (/), (/))

--sicpConstraint =
--  proc (c, f) -> do
