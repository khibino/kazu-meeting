--
-- $Header$
--

module Algebra (module Algebra) where


type Var = String

data Formula num = Num num
                 | Var Var
                 | Formula num :+ Formula num
                 | Formula num :* Formula num
                 | Formula num :^ Formula num
--                 | Neg { negateArg :: Formula num }
                 | Log (Formula num)
                 deriving (Show, Eq)
                          
infixl 6 :+, +!
infixl 7 :*, *!
infixr 8 :^, ^!
  
zero = Num 0
one  = Num 1

zero, one :: Num num => Formula num

minus :: Num num => num -> Formula num
minus x = Num (-x)

-- special variable e is natural logalithm base
-- 変数eは自然対数の底として特別あつかいする
baseE :: Formula num
baseE  = Var "e"
  
numberP :: Formula num -> Bool
numberP = p
  where p (Num _) = True
        p _       = False

variableP :: Formula num -> Bool
variableP = p
  where p (Var _) = True
        p _       = False

sameValiableP :: Formula num -> Formula num -> Bool
sameValiableP = p
  where p (Var a) (Var b) = a == b
        p _            _            = False

sumP :: Formula num -> Bool
sumP = p
  where p (_ :+ _) = True
        p _        = False

makeSum :: Num num => Formula num -> Formula num -> Formula num
makeSum = f
  where f (Num 0) a2      = a2
        f a1      (Num 0) = a1
        f (Num a) (Num b) = Num (a + b)
        f a1      a2      = a1 :+ a2

productP :: Formula num -> Bool
productP = p
  where p (_ :* _) = True
        p _        = False

makeProduct :: Num num => Formula num -> Formula num -> Formula num
makeProduct = f
  where f (Num 0)    _          = zero
        f _          (Num 0)    = zero
        f (Num 1)    m2         = m2
        f (Num (-1)) (Num b)    = minus b
        f m1         (Num 1)    = m1
        f (Num a)    (Num (-1)) = minus a
        f (Num a)  (Num b)  = Num (a * b)
        f m1      m2        = m1 :* m2
        
exponentiationP :: Formula num -> Bool
exponentiationP = p
  where p (_ :^ _) = True
        p _         = False

makeExponentiation :: (Ord num, Num num) => Formula num -> Formula num -> Formula num
makeExponentiation = f
  where f _       (Num 0) = one
        f (Num 0) (Num a)
          | a > 0           = zero
          | otherwise       = undefined
        f (Num 1) _         = one
        f b         (Num 1) = b
        f b         (ea :+ eb) = makeProduct (f b ea) (f b eb)
        f (Var "e") (Log a) = a
        f (Var "e") (ea :* Log eb) = f eb ea
        f (Var "e") (Log ea :* eb) = f ea eb
        f b       e       = b :^ e

-- negateP = p
--   where p (Neg _) = True
--         p _       = False

-- makeNegate = f
--   where f (Num a) = minus a
--         f _       = 

logarithmP :: Formula num -> Bool
logarithmP  = p
  where p (Log _) = True
        p _       = False
        
makeLogarithm :: (Ord num, Num num) => Formula num -> Formula num
makeLogarithm  = f
  where f (Num a)
          | a > 0          = Log $ Num a
          | otherwise      = undefined
        f (Var "e")        = one
        f ((Var "e") :^ e) = e
        f (b         :^ e) = makeProduct e $ makeLogarithm b
        f l                = Log l

-- deriv0 :: Num num => Formula num -> Formula num -> Formula num
-- deriv0 expr var
--   | numberP   expr = Num 0
--   | variableP expr = Num (if sameValiableP expr var then 1 else 0)
--   | sumP      expr = makeSum (deriv0 (addend expr) var) (deriv0 (augend expr) var)
--   | productP  expr = makeSum
--                     (makeProduct
--                      (multiplier expr)
--                      (deriv0 (multiplicand expr) var))
--                     (makeProduct
--                      (deriv0 (multiplier expr) var)
--                      (multiplicand expr))
--   | otherwise     = error ("unknown expression type -- DERIV " ++ show expr)


(+!) = makeSum
(*!) = makeProduct
(^!) = makeExponentiation

(+!), (*!), (^!) :: (Ord num, Num num) => Formula num -> Formula num -> Formula num


deriv :: (Ord num, Num num) => Formula num -> Var -> Formula num
deriv expr var = rec expr
  where rec (Num _)    = Num 0
        rec (Var nm)
          | nm == var  = Num 1
          | otherwise  = Num 0
--        rec (a :+ b) = makeSum (rec a) (rec b)
        rec (a :+ b) = rec a +! rec b
--        rec (a :* b) = makeSum (makeProduct a (rec b)) (makeProduct (rec a) b)
        rec (a :* b) = a *! rec b +! rec a *! b
        rec (a :^ b) = baseE ^! (b *! logA) *! (rec b *! logA +! b *! a ^! minus 1 *! rec a)
          where logA   = makeLogarithm a
        rec (Log  a) = makeProduct (rec a) $ makeExponentiation (Var "e") $ makeProduct (Num (-1)) a

--        rec aexpr       = error ("unknown expression type -- DERIV" ++ show aexpr)

-- deriv (Prod (Prod (Var "x") (Var "y")) (Var "x")) "x"

-- (a x  ^ b x)' --> ((e^)(b x * (log.a) x))'
-- --  h x = b x * (log.a) x
-- ((e^)(b x  * (log.a) x))'
--  --> (((e^).h) x)'
--  --> ((e^)'.h) x * h' x
-- -- (e^)' y = (e^) y, h' x = b' x * (log.a) x + b x * ((log.a) x)'
-- --                        = b' x * (log.a) x + b x * a x ^ (-1) * a' x
-- ((e^)'.h) x * h' x
--  --> (e^) (b x * (log.a) x) * (b' x * (log.a) x + b x * a x ^ (-1) * a' x)

