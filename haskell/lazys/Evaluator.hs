
module Evaluator where

import Data.List (find)
import qualified Syntax as S

mainNotFound :: a
mainNotFound =  error "main definition not found."

simpleProgram :: Num n => S.Module n -> S.Program n
simpleProgram m =
  maybe mainNotFound
  (\e -> S.Program { S.entry = e, S.modules = [m]}) -- (`S.Program` m)
  $ find undefined m

type Var = String

data Literal n = Num n
               | Str String
               | List [Result n]

instance Show n => Show (Literal n) where
  show (Num n)   = show n
  show (Str str) = show str
  show (List list') = show list'

data Value n = Lit (Literal n)
               deriving Show

data Result n = Value (Value n)
              | IO (IO (Value n))
              | Closure { closureCode    :: S.Lambda n
                        , closureVarArgs :: [Result n]
                        , closureEnv     :: Env n
                        }
                
instance Show n => Show (Result n) where
  show (Value v) = show v
  show (IO _)    = "<IO>"
  show (Closure _ _ _) = "<Closure>"

list :: Num n => [Result n] -> Result n
list =  Value . Lit . List

data Bind n = Bind { bindKey   :: Var
                   , bindValue :: Result n
                   }

type Env n = [Bind n]

evalError :: String -> String -> a 
evalError s = error . (s ++) . (": " ++)

applyError :: String -> a
applyError =  evalError "apply"

unboundVariable :: String -> a
unboundVariable =  applyError . (++ ": binding not found!")

apply :: Num n => Result n -> [Result n] -> Result n
apply fun                             []   = fun
apply (Closure lambda' _ env') args =
  if   restLen > 0 then partial
  else full
    where argsLen = length args
          params = S.params lambda'
          paramsLen = S.paramsLen lambda'
          restLen = paramsLen - argsLen
          --closure l v e = Closure l v e
          bind a = Bind (S.runPVar a)

          partial =
            let (toBind, rest) = splitAt argsLen params
                env'' = zipWith bind toBind args ++ env' in
            Closure (lambda' { S.params = rest, S.paramsLen = restLen }) [] env''

          full =
            let (toBind, rest) = splitAt paramsLen args
                env'' = zipWith bind params toBind ++ env' in
            case S.varParam lambda' of
              Just _  -> Closure (lambda' { S.params = [], S.paramsLen = 0 }) rest env''
              Nothing -> apply (evalExp' env'' $ S.body lambda') rest
apply _ _ = applyError "not function type"

isFilled :: Num n => S.Lambda n -> Bool
isFilled lambda' | S.params lambda' == [] && S.paramsLen lambda' == 0 = True
                 | S.params lambda' == []   = error "Inconsistent param."
                 | S.paramsLen lambda' == 0 = error "Inconsistent param."
                 | otherwise = False

-- 可変引数関数の呼び出し
vcall :: Num n => Result n -> Result n
vcall (Closure lambda' varArgs' env')
  | Just var <- S.varParam lambda', isFilled lambda' =
    evalExp' (Bind var (list varArgs') : env') (S.body lambda')
  | otherwise = error "Not variable param or not filled param closure is passed vcall!"
vcall _ =  error "Not closure is passwd vcall!"


evalLit :: Num n => S.Literal n -> Literal n
evalLit =  f
  where f (S.Num n) = Num n
        f (S.Str s) = Str s
        f (S.Quote _) = undefined

matchPattern :: Num n => S.Pat -> Result n -> [Bind n]
matchPattern pat value' = dispatch pat
  where dispatch (S.PVar var) = [Bind var value']

evalExp' :: Num n => Env n -> S.Exp n -> Result n
evalExp' env = eval
  where eval (S.Lit lit)      = Value $ Lit $ evalLit lit
        eval (S.EVar var) =
          maybe (unboundVariable var) bindValue
          $ find ((var ==) . bindKey) env
        eval (S.FApp fun args) =
          apply (eval fun) (map eval args)
        eval (S.VCall clo) =
          vcall $ eval clo
        eval (S.Abs lambda) = closure lambda env
        eval (S.Let binds expr) = evalExp' envLet expr
          where envLet = foldr extendEnv env binds
                extendEnv (S.BPat pat expr')  env' = matchPattern pat (evalExp' envLet expr') ++ env'
                extendEnv (S.BFun var lambda) env' = Bind var (closure lambda envLet) : env'
          
        closure lambda env' = Closure { closureCode = lambda, closureVarArgs = [], closureEnv = env' }

evalExp :: Num n => S.Exp n -> Result n
evalExp =  evalExp' []
