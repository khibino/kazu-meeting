module Parser (
  toExpr) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Instances ()

import PrimNum (PNum)

import SExpSyntax (SExp' (..), SExp)
import qualified SExpSyntax as SExp (Atom(..), toList, toList1)

import Syntax (Var, Pat(..), lambda, number, string, quote)
import qualified Syntax (Literal(..), Lambda(..), Exp(..), Bind(..), Module)

type ParseResult exp = Either String exp
errorResult :: String -> ParseResult exp
errorResult =  Left

parseError :: String -> SExp -> ParseResult exp
parseError name = errorResult . (("Ill-formed " ++ name ++ "!: ") ++) . show

successResult :: exp -> ParseResult exp
successResult =  return


type Literal = Syntax.Literal PNum
type Lambda  = Syntax.Lambda PNum
type Exp     = Syntax.Exp PNum
type Bind    = Syntax.Bind PNum
type Module  = Syntax.Module PNum

toPat :: SExp -> ParseResult Pat
toPat =  match
  where match (Atom (SExp.Id var)) = successResult $ PVar var
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
  where match (params :! expr :! Nil) =
          uncurry lambda <$> toLambdaParams params <*> toExpr expr
        match  form = parseError "lambda" form

toExpr :: SExp -> ParseResult Exp
toExpr =  rec
  where atom (SExp.Num n)  = number n
        atom (SExp.Str s)  = string s
        atom (SExp.Id id') = Syntax.EVar id'
        rec  Nil     = successResult $ quote Nil
        rec (Atom a) = successResult $ atom a
        rec form@(Atom (SExp.Id "let") :! rest)
          | (binds :! expr :! Nil) <- rest = Syntax.Let
                                             <$> (SExp.toList binds >>= mapM toBind)
                                             <*> rec expr
          | otherwise                     = parseError "let" form
        rec (Atom (id'@(SExp.Id sym)) :! rest)
          | sym `elem` ["lambda", "\\"] = Syntax.Abs <$> toLambda rest
          | _ :! _ <- rest              = Syntax.FApp  (atom id') <$> toExprList rest
          | Nil    <- rest              = successResult $ Syntax.VCall (atom id')
        rec form@(Atom _ :! _)         = parseError "function call expression" form
        rec (form :! Nil)  = Syntax.VCall <$> rec form
        rec (proc :! args) = Syntax.FApp <$> rec proc <*> toExprList args

toBind :: SExp -> ParseResult Bind
toBind =  dispatch
  where dispatch (pat@(Atom (SExp.Id _)) :! expr :! Nil) =
          Syntax.BPat <$> toPat pat <*> toExpr expr
        dispatch ((Atom (SExp.Id var) :! params) :! body) =
          Syntax.BFun var <$> toLambda (params :! body)
        dispatch  form = parseError "bind" form
