--
-- $Header$
--

module Algebra (module Algebra) where


data Formula num =
  Number num |
  Variable String |
  Sum  { addend :: Formula num, augend :: Formula num } |
  Prod { multiplier   :: Formula num, multiplicand :: Formula num }
  deriving Show
  
  
numberP :: Formula num -> Bool
numberP = p
  where p (Number _) = True
        p _          = False

variableP :: Formula num -> Bool
variableP = p
  where p (Variable _) = True
        p _            = False

sameValiableP :: Formula num -> Formula num -> Bool
sameValiableP = p
  where p (Variable a) (Variable b) = a == b
        p _            _            = False

sumP :: Formula num -> Bool
sumP = p
  where p (Sum _ _) = True
        p _         = False

-- makeSum :: Formula num -> Formula num -> Formula num
-- makeSum = Sum

productP :: Formula num -> Bool
productP = p
  where p (Prod _ _) = True
        p _          = False

-- makeProduct :: Formula num -> Formula num -> Formula num
-- makeProduct = Prod

deriv0 :: Num num => Formula num -> Formula num -> Formula num
deriv0 expr var
  | numberP   expr = Number 0
  | variableP expr = Number (if sameValiableP expr var then 1 else 0)
  | sumP      expr = Sum (deriv0 (addend expr) var) (deriv0 (augend expr) var)
  | productP  expr = Sum
                    (Prod
                     (multiplier expr)
                     (deriv0 (multiplicand expr) var))
                    (Prod
                     (deriv0 (multiplier expr) var)
                     (multiplicand expr))
  | otherwise     = error ("unknown expression type -- DERIV " ++ show expr)
                    
                         
deriv :: Num num => Formula num -> Formula num -> Formula num
deriv expr var = rec expr
  where rec (Number _)           = Number 0
        rec (Variable a)         = case var of
          (Variable b) | a == b -> Number 1
          _                     -> Number 0
        rec (Sum  a b)           = Sum (rec a) (rec b)
        rec (Prod a b)           = Sum (Prod a (rec b)) (Prod (rec a) b)
--        rec aexpr       = error ("unknown expression type -- DERIV" ++ show aexpr)
