module TestEval where

import Syntax
import Evaluator (evalExp, Result)

exp0 :: Exp Integer
exp0 =  Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x")

test0 :: Result Integer
test0 =  evalExp exp0
