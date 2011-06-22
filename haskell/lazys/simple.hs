

import Control.Applicative ((<$>))

import PrimNum (PNum)
import ParseResult (ParseResult)
import qualified SExpParser as SExp (parseExpr)
import Parser (parseExpr)
import Evaluator (evalExp, Result)

run :: String -> ParseResult (Result PNum)
run sexpr = evalExp <$>
            (SExp.parseExpr sexpr
             >>= parseExpr)

main :: IO ()
main = (run <$> getContents) >>= print
