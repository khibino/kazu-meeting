module SExpTest (
  t1,
  test0, test1, test2
  ) where

import ParseResult (ParseResult)
import SExpSyntax (SExp)
import SExpParser (parseExpr)

--parseE = parseExpr

t1 = parseExpr "(x)"

test0 =  parseExpr "(let ((x 1)) x)"

test1 =  parseExpr "(let ((x 1) (y x)) y)"

test2 =  parseExpr "(let ((x 1) (y . (x))) y)"
--test2 = undefined
 
t1, test0, test1, test2 :: ParseResult SExp
