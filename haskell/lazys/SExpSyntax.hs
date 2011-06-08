module SExpSyntax (
  Atom(..), SExp'(..), SExp,
  list,
  string, symbol,
  SNum,
  integer, double,
  readInteger, readDouble,
  quote) where

data Atom n = Num n
            | Str String
            | Id String
            deriving Eq
            --deriving Show

showAtom :: (Num n, Show n) => Atom n -> String
showAtom = f
  where f (Num n) = tag "num" $ show n
        f (Str str) = show str
        f (Id s)  = tag "symbol" s
        tag n s = '<':n ++  ':':s ++ ">"

instance (Show n, Num n) => Show (Atom n) where
  show = showAtom

data SExp' a = Atom a
             | (SExp' a) :! (SExp' a)
             | Nil
             deriving (Show, Eq)

infixr 5 :!

list :: [SExp' a] -> SExp' a
list = foldr (:!) Nil

data SNum = I Integer
          | F Double
            
instance Eq SNum where
  I a == I b = a == b
  F a == F b = a == b
  I a == b   = F (fromIntegral a) == b
  a   == I b = a == F (fromIntegral b)
  
instance Show SNum where
  show (I a) = show a
  show (F a) = show a


type Op2 a b = a -> a -> b
type IntegerOp2 = Op2 Integer Integer
type DoubleOp2  = Op2 Double  Double

type UniOp a = a -> a
type IntegerUni = UniOp Integer
type DoubleUni  = UniOp Double

upcastOp2 :: IntegerOp2 -> DoubleOp2 -> SNum -> SNum -> SNum
upcastOp2 iOp dOp = f
  where f (I a) (I b) = I (iOp a b)
        f (F a) (F b) = F $ dOp a b
        f (I a) b     = f (F (fromIntegral a)) b
        f a     (I b) = f a (F (fromIntegral b))

uniOp :: IntegerUni -> DoubleUni -> SNum -> SNum
uniOp iOp dOp = f
  where f (I n) = I (iOp n)
        f (F n) = F (dOp n)

instance Num SNum where
  (+) = upcastOp2 (+) (+)
  (*) = upcastOp2 (*) (*)
  (-) = upcastOp2 (-) (-)
  negate = uniOp negate negate
  abs    = uniOp abs abs
  signum = uniOp signum signum
  fromInteger = I

num :: a -> SExp' (Atom a)
num = Atom . Num

type SExp = SExp' (Atom SNum)


integer :: Integer -> SExp
integer = num . I

double :: Double -> SExp
double  = num . F

readInteger :: String -> SExp
readInteger =  integer . (read :: String -> Integer)

readDouble :: String -> SExp
readDouble =  double . (read :: String -> Double)

string :: String -> SExp
string =  Atom . Str

symbol :: String -> SExp
symbol =  Atom . Id

quoteId :: SExp
quoteId =  Atom (Id "quote")

quote :: SExp -> SExp
quote = (quoteId :!) . (:! Nil)
