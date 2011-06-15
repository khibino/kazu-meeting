module EvalTest where

import Syntax
import Evaluator (evalExp, Result, run)

exp0 :: Exp' Integer
exp0 =  Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x")

test0 :: Result Integer
test0 =  evalExp exp0

mod1 :: Module' Integer
mod1 =  Module "Main" [BPat
                       (PVar "main")
                       (Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x"))]

test1 :: Result Integer
test1 = run [] mod1

--exp2 :: 
--ext2 =  
