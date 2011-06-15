
module Syntax (
  Var, Mod,
  
  Pat(..), 
  Literal, Literal'(..),
  Lambda, Lambda'(..), lambda,
  
  Exp, Exp'(..),
  number, string, quote,
  
  Bind, Bind'(..),
  Module, Module'(..),
  --Program(..), simpleProgram
  ) where

import PrimNum (PNum)
import SExpSyntax (SExp)

type Var = String
type Mod = String

data Pat = PVar { runPVar :: Var }
--         |
         deriving (Eq, Show, Read)

data Literal' n = Num n
                | Str String
                | SExp SExp
                deriving (Eq, Show)

data Lambda' n = Lambda { params :: [Pat]
                        , paramsLen :: Int
                        , varParam :: Maybe Var
                        , body :: Exp' n
                        }
              deriving (Eq, Show)

lambda :: [Pat] -> Maybe Var -> Exp' n -> Lambda' n
lambda params' =
  Lambda params' (length params')

data Exp' n = Lit (Literal' n)
            | EVar Var
            | FApp (Exp' n) [Exp' n]
            | VCall (Exp' n) -- 可変引数関数の呼び出し
            | Abs (Lambda' n)
            | Let [Bind' n] (Exp' n)
            deriving (Eq, Show)

number :: n -> Exp' n
number =  Lit . Num

string :: String -> Exp' n
string =  Lit . Str

quote :: SExp -> Exp' n
quote =  Lit . SExp

data Bind' n = BPat Pat (Exp' n)
             | BFun Var (Lambda' n)
             deriving (Eq, Show)

data Module' n = Module { name :: Mod
                        , binds :: [Bind' n]
                        }
              deriving (Eq, Show)

--data Program n = Program { modules :: [Module n] }
--               deriving (Eq, Show)

--simpleProgram :: Module n -> Program n
--simpleProgram =  Program . (:[])

type Literal = Literal' PNum
type Lambda  = Lambda'  PNum
type Exp     = Exp'  PNum
type Bind    = Bind' PNum
type Module  = Module' PNum
