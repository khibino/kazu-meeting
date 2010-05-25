--
-- $Header$
--

module Algebra (module Algebra) where


data Formula num = Num num
                 | Var String
                 | Sum  { addend :: Formula num, augend :: Formula num }
                 | Prod { multiplier   :: Formula num, multiplicand :: Formula num }
                 deriving (Show, Eq)
  
  
numberP :: Formula num -> Bool
numberP = p
  where p (Num _) = True
        p _          = False

variableP :: Formula num -> Bool
variableP = p
  where p (Var _) = True
        p _            = False

sameValiableP :: Formula num -> Formula num -> Bool
sameValiableP = p
  where p (Var a) (Var b) = a == b
        p _            _            = False

sumP :: Formula num -> Bool
sumP = p
  where p (Sum _ _) = True
        p _         = False

makeSum :: Num num => Formula num -> Formula num -> Formula num
makeSum = f
  where f (Num 0) a2      = a2
        f a1      (Num 0) = a1
        f (Num a) (Num b) = Num (a + b)
        f a1      a2      = Sum a1 a2

productP :: Formula num -> Bool
productP = p
  where p (Prod _ _) = True
        p _          = False

makeProduct :: Num num => Formula num -> Formula num -> Formula num
makeProduct = f
  where f (Num 0) _       = Num 0
        f _       (Num 0) = Num 0
        f (Num 1) m2      = m2
        f m1      (Num 1) = m1
        f (Num a) (Num b) = Num (a * b)
        f m1      m2      = Prod m1 m2

deriv0 :: Num num => Formula num -> Formula num -> Formula num
deriv0 expr var
  | numberP   expr = Num 0
  | variableP expr = Num (if sameValiableP expr var then 1 else 0)
  | sumP      expr = makeSum (deriv0 (addend expr) var) (deriv0 (augend expr) var)
  | productP  expr = makeSum
                    (makeProduct
                     (multiplier expr)
                     (deriv0 (multiplicand expr) var))
                    (makeProduct
                     (deriv0 (multiplier expr) var)
                     (multiplicand expr))
  | otherwise     = error ("unknown expression type -- DERIV " ++ show expr)
                    
                         
deriv :: Num num => Formula num -> String -> Formula num
deriv expr var = rec expr
  where rec (Num _)    = Num 0
        rec (Var nm)
          | nm == var  = Num 1
          | otherwise  = Num 0
        rec (Sum  a b) = makeSum (rec a) (rec b)
        rec (Prod a b) = makeSum (makeProduct a (rec b)) (makeProduct (rec a) b)
--        rec aexpr       = error ("unknown expression type -- DERIV" ++ show aexpr)

-- deriv (Prod (Prod (Var "x") (Var "y")) (Var "x")) "x"
