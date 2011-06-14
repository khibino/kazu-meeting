module SExpTest (
  test0, test1, test2
  ) where

import SExpSyntax (SExp)
import SExpParser (parseExpr)

test0 :: [(SExp, String)]
test0 =  parseExpr "(let ((x 1)) x)"

test1 :: [(SExp, String)]
test1 =  parseExpr "(let ((x 1) (y x)) y)"

test2 = undefined
