module EvalTest where

import Control.Applicative ((<$>))

import PrimNum (PNum)
import ParseResult (ParseResult)
import qualified SExpParser as SExp (parseExpr)
import Syntax (Literal'(..), Pat(..),
               Exp, Exp'(..), Bind'(..),
               Module, Module'(..))
import Parser (parseExpr)
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


runTest :: String -> ParseResult (Result PNum)
runTest sexpr = evalExp <$>
                (SExp.parseExpr sexpr
                 >>= parseExpr)

test2 :: ParseResult (Result PNum)
test2 =  runTest "(let ((x 1)) x)"


test3 :: ParseResult (Result PNum)
test3 =  runTest "(let ((x 1) (y x)) y)"

test4 :: ParseResult (Result PNum)
test4 =  runTest "(let ((x 3)) (lambda (y) (y x)))"

test5 :: ParseResult (Result PNum)
test5 =  runTest "(let ((x 1) (y (+ x 3)) (z (/ y 5))) z)"

test6 :: ParseResult (Result PNum)
test6 =  runTest "(let (((tru x y) x) \
                     \ ((fls x y) y) \
                     \ ((if p x y) (p x y))) \
                   \ (if fls (+ 2 3) (* 2 3)))"

test7 :: ParseResult (Result PNum)
test7 =  runTest "(if (< 2 1) (+ 3 5) (* 3 5))"


