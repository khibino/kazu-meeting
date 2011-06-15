module EvalTest where

import PrimNum (PNum)
import Syntax (Literal'(..), Pat(..),
               Exp, Exp'(..), Bind'(..),
               Module, Module'(..))
import Evaluator (evalExp, Result, run)

exp0 :: Exp
exp0 =  Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x")

test0 :: Result PNum
test0 =  evalExp exp0

mod1 :: Module
mod1 =  Module "Main" [BPat
                       (PVar "main")
                       (Let [BPat (PVar "x") (Lit (Num 1))] (EVar "x"))]

test1 :: Result PNum
test1 =  run [] mod1

--exp2 :: 
--ext2 =  
