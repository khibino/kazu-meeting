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
  
(+!) :: Num num => Formula num -> Formula num -> Formula num
(+!) = f
  where f (Num 0) a2      = a2
        f a1      (Num 0) = a1
        f (Num a) (Num b) = Num (a + b)
        f a1      a2      = a1 :+ a2

(*!) :: Num num => Formula num -> Formula num -> Formula num
(*!) = f
  where f (Num 0)    _          = zero
        f _          (Num 0)    = zero
        f (Num 1)    m2         = m2
        f (Num (-1)) (Num b)    = minus b
        f m1         (Num 1)    = m1
        f (Num a)    (Num (-1)) = minus a
        f (Num a)  (Num b)  = Num (a * b)
        f m1      m2        = m1 :* m2
        
(^!) :: (Ord num, Num num) => Formula num -> Formula num -> Formula num
(^!) = f
  where f _       (Num 0) = one
        f (Num 0) (Num a)
          | a > 0           = zero
          | otherwise       = undefined
        f (Num 1) _         = one
        f b         (Num 1) = b
        f b         (ea :+ eb) = f b ea *! f b eb
        f (Var "e") (Log a) = a
        f (Var "e") (ea :* Log eb) = f eb ea
        f (Var "e") (Log ea :* eb) = f ea eb
        f b       e       = b :^ e

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
        f (Var "e" :^ e) = e
        f (b         :^ e) = e *! makeLogarithm b
        f l                = Log l

--(*!) = makeProduct
--(^!) = makeExponentiation

--(^!) :: (Ord num, Num num) => Formula num -> Formula num -> Formula num


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
        rec (Log  a) = rec a *! Var "e" ^! Num (-1) *! a

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
