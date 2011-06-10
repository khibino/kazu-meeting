module Parser (
  toExpr) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Instances ()

import PrimNum (PNum)

import SExpSyntax (SExp'((:!)), SExp)
import qualified SExpSyntax as SExp (SExp'(Atom, Nil), Atom(..), toList, toList1)

import Syntax (Var, Pat(..), lambda, number, string, quote,
               Literal'(..), Lambda'(..), Exp'(..), Bind'(..), Module'(..))
--import Syntax hiding (Literal, Lambda, Exp, Bind, Module)

type ParseResult exp = Either String exp
errorResult :: String -> ParseResult exp
errorResult =  Left

parseError :: String -> SExp -> ParseResult exp
parseError formKind = errorResult . (("Ill-formed " ++ formKind ++ "!: ") ++) . show

successResult :: exp -> ParseResult exp
successResult =  return


type Literal = Literal' PNum
type Lambda  = Lambda'  PNum
type Exp     = Exp'  PNum
type Bind    = Bind' PNum
type Module  = Module' PNum

toPat :: SExp -> ParseResult Pat
toPat =  match
  where match (SExp.Atom (SExp.Id var)) = successResult $ PVar var
        match  form                = parseError "pattern" form

toLambdaParams :: SExp -> ParseResult ([Pat], Maybe Var)
toLambdaParams sExp =
  let (pats, varArg) = SExp.toList1 sExp in
  (,)
  <$> mapM toPat pats
  <*> maybe (successResult Nothing) (fmap (Just . runPVar) . toPat) varArg

toExprList :: SExp -> ParseResult [Exp]
toExprList =  (>>= mapM toExpr) . SExp.toList

toLambda :: SExp -> ParseResult Lambda
toLambda =  match
  where match (params' :! expr :! SExp.Nil) =
          uncurry lambda <$> toLambdaParams params' <*> toExpr expr
        match  form = parseError "lambda" form

toExpr :: SExp -> ParseResult Exp
toExpr =  rec
  where atom (SExp.Num n)  = number n
        atom (SExp.Str s)  = string s
        atom (SExp.Id id') = Syntax.EVar id'
        rec  SExp.Nil     = successResult $ quote SExp.Nil
        rec (SExp.Atom a) = successResult $ atom a
        rec form@(SExp.Atom (SExp.Id "let") :! rest)
          | (binds' :! expr :! SExp.Nil) <- rest = Syntax.Let
                                                   <$> (SExp.toList binds' >>= mapM toBind)
                                                   <*> rec expr
          | otherwise                     = parseError "let" form
        rec (SExp.Atom (id'@(SExp.Id sym)) :! rest)
          | sym `elem` ["lambda", "\\"] = Syntax.Abs <$> toLambda rest
          | _ :! _   <- rest              = Syntax.FApp  (atom id') <$> toExprList rest
          | SExp.Nil <- rest              = successResult $ Syntax.VCall (atom id')
        rec form@(SExp.Atom _ :! _)         = parseError "function call expression" form
        rec (form :! SExp.Nil)  = Syntax.VCall <$> rec form
        rec (proc :! args)      = Syntax.FApp <$> rec proc <*> toExprList args

toBind :: SExp -> ParseResult Bind
toBind =  dispatch
  where dispatch (pat@(SExp.Atom (SExp.Id _)) :! expr :! SExp.Nil) =
          Syntax.BPat <$> toPat pat <*> toExpr expr
        dispatch ((SExp.Atom (SExp.Id var) :! params') :! body') =
          Syntax.BFun var <$> toLambda (params' :! body')
        dispatch  form = parseError "bind" form
