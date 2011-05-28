
module Syntax where

type Var = String

data Pat = PVar { runPVar :: Var }
--         |
         deriving (Eq, Show, Read)

data SExp = SExp () deriving (Eq, Show, Read)

data Literal n = Num n
               | Str String
               | Quote SExp
               deriving (Eq, Show, Read)

data Lambda n = Lambda { params :: [Pat]
                       , paramsLen :: Int
                       , varParam :: Maybe Var
                       , body :: Exp n
                       }
              deriving (Eq, Show, Read)
              

data Exp n = Lit (Literal n)
           | EVar Var
           | FApp (Exp n) [Exp n]
           | VCall (Exp n) -- 可変引数関数の呼び出し
           | Abs (Lambda n)
           | Let [Bind n] (Exp n)
           deriving (Eq, Show, Read)

data Bind n = BPat Pat (Exp n)
            | BFun Var (Lambda n)
            deriving (Eq, Show, Read)

type Module n = [Bind n]

data Program n = Program { entry   ::  Bind n
                         , modules :: [Module n]
                         }
               deriving (Eq, Show, Read)
