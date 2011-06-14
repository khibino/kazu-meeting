module SExpTest (
  t1,
  test0, test1, test2
  ) where

--import Text.ParserCombinators.ReadP(readP_to_S)
import SExpSyntax (SExp)
import SExpParser (parseFloat, parseAtom,
                   parseExpr, parseExprList)

--parseE = parseExpr

t1 = parseExpr "(x)"

test0 =  parseExpr "(let ((x 1)) x)"

test1 =  parseExpr "(let ((x 1) (y x)) y)"

test2 =  parseExpr "(let ((x 1) (y . (x))) y)"
--test2 = undefined
 
t1, test0, test1, test2 :: [(SExp, String)]
