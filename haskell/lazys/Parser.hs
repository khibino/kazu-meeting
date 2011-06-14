module Parser (
  toExpr,
  toModule
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Instances ()

import PrimNum (PNum)

import SExpSyntax (SExp'((:!)), SExp)
import qualified SExpSyntax as SExp

import Syntax (Var, Pat(..), lambda, number, string, quote,
               Lambda'(..), Exp'(..), Bind'(..), Module'(..))

type ParseResult exp = Either String exp

(<|>) :: ParseResult exp -> ParseResult exp -> ParseResult exp
(Left  _)   <|> b = b
a@(Right _) <|> _ = a

infixl 3 <|>

errorResult :: String -> ParseResult exp
errorResult =  Left

parseError :: String -> SExp -> ParseResult exp
parseError formKind = errorResult . (("Ill-formed " ++ formKind ++ "!: ") ++) . show

successResult :: exp -> ParseResult exp
successResult =  return


--type Literal = Literal' PNum
type Lambda  = Lambda'  PNum
type Exp     = Exp'  PNum
type Bind    = Bind' PNum
type Module  = Module' PNum

type Parser e = SExp -> ParseResult e

many :: Parser a -> Parser [a]
many p = (>>= mapM p) . SExp.toList

toPat :: Parser Pat
toPat =  match
  where match (SExp.Atom (SExp.Id var)) = successResult $ PVar var
        match  form                = parseError "pattern" form

toLambdaParams :: Parser ([Pat], Maybe Var)
toLambdaParams sExp =
  let (pats, varArg) = SExp.toList1 sExp in
  (,)
  <$> mapM toPat pats
  <*> maybe (successResult Nothing) (fmap (Just . runPVar) . toPat) varArg

toLambda :: Parser Lambda
toLambda =  match
  where match (params' :! expr :! SExp.Nil) =
          uncurry lambda <$> toLambdaParams params' <*> toExpr expr
        match  form = parseError "lambda" form

toExpr :: Parser Exp
toExpr =  rec
  where atom (SExp.Num n)  = number n
        atom (SExp.Str s)  = string s
        atom (SExp.Id id') = Syntax.EVar id'
        rec  SExp.Nil     = successResult $ quote SExp.Nil
        rec (SExp.Atom a) = successResult $ atom a
        rec form@(SExp.Atom (SExp.Id "let") :! rest)
          | (binds' :! expr :! SExp.Nil) <- rest = Syntax.Let
                                                   <$> many toBind binds'
                                                   <*> rec expr
          | otherwise                     = parseError "let" form
        rec (SExp.Atom (SExp.Id "quote") :! rest) = successResult $ quote rest
        rec (SExp.Atom (SExp.Id sym) :! rest)
          | sym `elem` ["lambda", "\\"] = Syntax.Abs <$> toLambda rest
        rec form@(proc :! SExp.Nil) | SExp.functionValue proc = Syntax.VCall <$> rec proc
                                    | otherwise               = parseError "varargs function call expression" form
        rec form@(proc :! args) | SExp.functionValue proc = Syntax.FApp <$> rec proc <*> many toExpr args
                                | otherwise               = parseError "function call expression" form


toBind :: Parser Bind
toBind =  dispatch
  where dispatch (pat@(SExp.Atom (SExp.Id _)) :! expr :! SExp.Nil) =
          Syntax.BPat <$> toPat pat <*> toExpr expr
        dispatch ((SExp.Atom (SExp.Id var) :! params') :! body') =
          Syntax.BFun var <$> toLambda (params' :! body')
        dispatch  form = parseError "bind" form

toModule :: [SExp] -> ParseResult Module
toModule ees@(e:es) = Module <$> modDecl e <*> topBinds es <|>
                       Module "Main" <$> topBinds ees
  where modDecl (SExp.Atom (SExp.Id "module")
                 :! SExp.Atom (SExp.Id name') :! SExp.Nil) =
          successResult name'
        modDecl  form = parseError "module declaration" form
        topBinds = mapM bindTop
        bindTop (SExp.Atom (SExp.Id "define") :! bind) = toBind bind
        bindTop  form = parseError "bind at top" form
toModule []         = parseError "module" SExp.Nil
