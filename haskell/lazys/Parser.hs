module Parser (
  parseExpr,
  parseModule
  ) where

import Control.Applicative ((<$>), (<*>))

import ParseResult (ParseResult, (<|>), errorResult, successResult)
import SExpSyntax (SExp'((:!)), SExp)
import qualified SExpSyntax as SExp

import Syntax (Var, Pat(..), lambda, number, string, quote,
               Lambda,
               Exp, Exp'(..),
               Bind, Bind'(..),
               Module, Module'(..))

parseError :: String -> SExp -> ParseResult exp
parseError formKind = errorResult . (("Ill-formed " ++ formKind ++ "!: ") ++) . show


type Parser e = SExp -> ParseResult e

many :: Parser a -> Parser [a]
many p = (>>= mapM p) . SExp.toList

parsePat :: Parser Pat
parsePat =  match
  where match (SExp.Atom (SExp.Id var)) = successResult $ PVar var
        match  form                = parseError "pattern" form

parseLambdaParams :: Parser ([Pat], Maybe Var)
parseLambdaParams sExp =
  let (pats, varArg) = SExp.toList1 sExp in
  (,)
  <$> mapM parsePat pats
  <*> maybe (successResult Nothing) (fmap (Just . runPVar) . parsePat) varArg

parseLambda :: Parser Lambda
parseLambda =  match
  where match (params' :! expr :! SExp.Nil) =
          uncurry lambda <$> parseLambdaParams params' <*> parseExpr expr
        match  form = parseError "lambda" form

parseExpr :: Parser Exp
parseExpr =  rec
  where atom (SExp.Num n)  = number n
        atom (SExp.Str s)  = string s
        atom (SExp.Id id') = Syntax.EVar id'
        rec  SExp.Nil     = successResult $ quote SExp.Nil
        rec (SExp.Atom a) = successResult $ atom a
        rec form@(SExp.Atom (SExp.Id "let") :! rest)
          | (binds' :! expr :! SExp.Nil) <- rest = Syntax.Let
                                                   <$> many parseBind binds'
                                                   <*> rec expr
          | otherwise                     = parseError "let" form
        rec (SExp.Atom (SExp.Id "quote") :! rest) = successResult $ quote rest
        rec (SExp.Atom (SExp.Id sym) :! rest)
          | sym `elem` ["lambda", "\\"] = Syntax.Abs <$> parseLambda rest
        rec form@(proc :! SExp.Nil) | SExp.functionValue proc = Syntax.VCall <$> rec proc
                                    | otherwise               = parseError "varargs function call expression" form
        rec form@(proc :! args) | SExp.functionValue proc = Syntax.FApp <$> rec proc <*> many parseExpr args
                                | otherwise               = parseError "function call expression" form


parseBind :: Parser Bind
parseBind =  dispatch
  where dispatch (pat@(SExp.Atom (SExp.Id _)) :! expr :! SExp.Nil) =
          Syntax.BPat <$> parsePat pat <*> parseExpr expr
        dispatch ((SExp.Atom (SExp.Id var) :! params') :! body') =
          Syntax.BFun var <$> parseLambda (params' :! body')
        dispatch  form = parseError "bind" form

parseModule :: [SExp] -> ParseResult Module
parseModule ees@(e:es) = Module <$> modDecl e <*> topBinds es <|>
                       Module "Main" <$> topBinds ees
  where modDecl (SExp.Atom (SExp.Id "module")
                 :! SExp.Atom (SExp.Id name') :! SExp.Nil) =
          successResult name'
        modDecl  form = parseError "module declaration" form
        topBinds = mapM bindTop
        bindTop (SExp.Atom (SExp.Id "define") :! bind) = parseBind bind
        bindTop  form = parseError "bind at top" form
parseModule []         = parseError "module" SExp.Nil
