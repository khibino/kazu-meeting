module TestEval where

import Syntax
import Evaluator (evalExp)

exp0  = Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x")

test0 = evalExp exp0
