module AlgebraD where

{-
問題 2.73
  
a. 
演算子無しのケースで分けても variable か num か決定できない。
少なくとも operator, operands 以外のインターフェースにする必要がある。

b.
プログラムに記述した - add' mul'

c.
プログラムに記述した - hat'

d.
プログラムに記述した - derivD

-}

type Var = String

data BinOp = Sum | Prod | Exp deriving Show

data UniOp = Log deriving Show

data Formula num = Num num
                 | Var Var
                 | Bin { operator :: BinOp
                       , leftOperand  :: Formula num
                       , rightOperand :: Formula num
                       }
                 | Uni { uniOperator :: UniOp, uniOperand :: Formula num }
                 deriving Show

infixl 6 +!
infixl 7 *!
infixr 8 ^!
  
zero = Num 0
one  = Num 1

zero, one :: Num num => Formula num

minus :: Num num => num -> Formula num
minus x = Num (-x)

-- special variable e is natural logalithm base
-- 変数eは自然対数の底として特別に扱う
baseE :: Formula num
baseE  = Var "e"
  

(+!) :: Num num => Formula num -> Formula num -> Formula num
(+!) = f
  where f (Num 0) a2      = a2
        f a1      (Num 0) = a1
        f (Num a) (Num b) = Num (a + b)
        f a1      a2      = Bin Sum a1 a2
(*!) :: Num num => Formula num -> Formula num -> Formula num
(*!) = f
  where f (Num 0)    _          = zero
        f _           (Num 0)   = zero
        f (Num 1)    m2         = m2
        f (Num (-1)) (Num b)   = minus b
        f m1          (Num 1)    = m1
        f (Num a)    (Num (-1)) = minus a
        f (Num a)    (Num b)   = Num (a * b)
        f m1          m2         = Bin Prod m1 m2
        
(^!) :: (Ord num, Num num) => Formula num -> Formula num -> Formula num
(^!) = f
  where f _       (Num 0) = one
        f (Num 0) (Num a)
          | a > 0           = zero
          | otherwise       = undefined
        f (Num 1) _         = one
        f b         (Num 1) = b
        f b         (Bin Sum ea eb) = f b ea *! f b eb
        f (Var "e") (Uni Log a) = a
        f (Var "e") (Bin Prod ea (Uni Log eb)) = f eb ea
        f (Var "e") (Bin Prod (Uni Log ea) eb) = f ea eb
        f b       e       = Bin Exp b e

makeLogarithm :: (Ord num, Num num) => Formula num -> Formula num
makeLogarithm  = f
  where f (Num a)
          | a > 0          = Uni Log $ Num a
          | otherwise      = undefined
        f (Var "e")        = one
        f (Bin Exp (Var "e") e) = e
        f (Bin Exp b         e) = e *! makeLogarithm b
        f l                = Uni Log l

-- rec (a :+ b) = rec a +! rec b
add' var a b = d a +! d b
  where d = (`deriv` var)

-- rec (a :* b) = a *! rec b +! rec a *! b
mul' var a b = a *! d b +! d a *! b
  where d = (`deriv` var)
        
-- rec (a :^ b) = baseE ^! (b *! logA) *! (rec b *! logA +! b *! a ^! minus 1 *! rec a)
hat' var a b = baseE ^! (b *! logA) *! (d b *! logA +! b *! a ^! minus 1 *! d a)
  where d = (`deriv` var)
        logA   = makeLogarithm a
        --   where logA   = makeLogarithm a
        -- rec (Log  a) = rec a *! Var "e" ^! Num (-1) *! a

add', mul', hat' :: (Ord num, Num num) => Var -> Formula num -> Formula num -> Formula num

data Operation = Deriv

sumM  Deriv = add'

prodM Deriv = mul'

expM Deriv = hat'

sumM, prodM, expM :: (Ord num, Num num) => Operation -> Var -> Formula num -> Formula num -> Formula num

deriv          :: (Ord num, Num num) => Formula num -> Var -> Formula num
deriv expr var =  d expr
  where d (Num _)  = Num 0
        d (Var fv) = Num (if var == fv then 1 else 0)
        d _         = getD (operator expr) var (leftOperand expr) (rightOperand expr)
        getD Sum  = add'
        getD Prod = mul'
        getD Exp  = hat'

derivD          :: (Ord num, Num num) => Formula num -> Var -> Formula num
derivD expr var =  d expr
  where d (Num _)  = Num 0
        d (Var fv) = Num (if var == fv then 1 else 0)
        d _        = get (operator expr) Deriv var (leftOperand expr) (rightOperand expr)
        get Sum  = sumM
        get Prod = prodM
        get Exp  = expM

-- deriv (Bin Prod (Bin Prod (Var "x") (Var "y")) (Var "x")) "x"
-- deriv (Bin Prod (Var "x") (Var "y")) (Var "x")) "x"
