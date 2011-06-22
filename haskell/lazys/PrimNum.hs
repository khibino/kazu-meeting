module PrimNum (
  PNum,
  fromDouble) where

import Data.Ord(comparing)
import Data.Ratio(numerator, denominator)

data PNum = I Integer
          | F Double

floatExpr :: Integer -> PNum
floatExpr =  F . fromInteger

upCast :: PNum -> PNum
upCast (I a) = floatExpr a
upCast    a  = a

instance Eq PNum where
  I a == I b = a == b
  F a == F b = a == b
  a   == b   = upCast a == upCast b
  
instance Ord PNum where
  compare (I a) (I b) = compare a b
  compare (F a) (F b) = compare a b
  compare    a     b  = comparing upCast a b

instance Show PNum where
  show (I a) = show a
  show (F a) = show a


type Op2 a b = a -> a -> b
type IntegerOp2 = Op2 Integer Integer
type DoubleOp2  = Op2 Double  Double

type UniOp a = a -> a
type IntegerUni = UniOp Integer
type DoubleUni  = UniOp Double

upcastOp2 :: IntegerOp2 -> DoubleOp2 -> PNum -> PNum -> PNum
upcastOp2 iOp dOp = f
  where f (I a) (I b) = I (iOp a b)
        f (F a) (F b) = F (dOp a b)
        f    a     b  = f (upCast a) (upCast b)
        -- f (I a) b     = f (F (fromInteger a)) b
        -- f a     (I b) = f a (F (fromInteger b))

uniOp :: IntegerUni -> DoubleUni -> PNum -> PNum
uniOp iOp dOp = f
  where f (I n) = I (iOp n)
        f (F n) = F (dOp n)

instance Num PNum where
  (+) = upcastOp2 (+) (+)
  (*) = upcastOp2 (*) (*)
  (-) = upcastOp2 (-) (-)
  negate = uniOp negate negate
  abs    = uniOp abs abs
  signum = uniOp signum signum
  fromInteger = I

instance Fractional PNum where
  --
  F a / F b = F (a / b)
  a   / b   = upCast a / upCast b
  --
  recip (F a) = F (recip a)
  recip    a  = recip (upCast a)
  --
  fromRational r = F (fromIntegral (numerator r) / 
                      fromIntegral (denominator r))

fromDouble :: Double -> PNum
fromDouble =  F
