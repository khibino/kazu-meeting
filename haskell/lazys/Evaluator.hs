
module Evaluator where

import Data.List (find)
import qualified Syntax as S

mainNotFound :: a
mainNotFound =  error "main definition not found."

type Var = String
type Mod = String

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

list :: [Result n] -> Result n
list =  Value . Lit . List

data Bind n = Bind { bindKey   :: Var
                   , bindValue :: Result n
                   }

type Env n = [Bind n]


evalError :: String -> String -> a
evalError s = error . (s ++) . (": " ++)


bindQualified :: Mod -> (Var, Result n) -> Bind n
bindQualified qual (var, value) = Bind (qual ++ "." ++ var) value

bind :: (Var, Result n) -> Bind n
bind =  uncurry Bind

extendEnv1 :: (Var, Result n) -> Env n -> Env n
extendEnv1 =  (:) . bind

extendEnv :: Env n -> [(Var, Result n)] -> Env n
extendEnv =  foldr extendEnv1

extendEnvTop :: Mod -> Env n -> [(Var, Result n)] -> (Env n, Env n)
extendEnvTop qual env pairs = (local, global)
  where global = foldr ((:) . bindQualified qual) env pairs
        local = foldr extendEnv1 global pairs

patternMatch :: S.Pat -> Result n -> [(Var, Result n)]
patternMatch pat value = dispatch pat
  where dispatch (S.PVar var) = [(var, value)]

paramsMatch :: [S.Pat] -> [Result n] -> Env n -> Env n
paramsMatch pats values env =
  extendEnv env
  $ concatMap (uncurry patternMatch) (zip pats values)

closure :: S.Lambda n -> Env n -> Result n
closure lambda env = Closure { closureCode = lambda, closureVarArgs = [], closureEnv = env }

bindMatch :: Env n -> S.Bind n -> [(Var, Result n)]
bindMatch env = match
  where match (S.BPat pat expr')  = patternMatch pat (evalExp' env expr')
        match (S.BFun var lambda) = [(var, closure lambda env)]

letEnv :: Env n -> [S.Bind n] -> Env n
letEnv env binds = recEnv'
  where recEnv' = extendEnv env $ concatMap match binds
        match = bindMatch recEnv'

topEnv :: Mod -> Env n -> [S.Bind n] -> (Env n, Env n)
topEnv qual env binds = envPair
  where envPair@(recEnv', _) = extendEnvTop qual env $ concatMap match binds
        match = bindMatch recEnv'

applyError :: String -> a
applyError =  evalError "apply"

unboundVariable :: String -> a
unboundVariable =  applyError . (++ ": binding not found!")

apply :: Result n -> [Result n] -> Result n
apply fun                      []   = fun
apply (Closure lambda _ env) args =
  if   restLen > 0 then partial
  else full
    where argsLen = length args
          params = S.params lambda
          paramsLen = S.paramsLen lambda
          restLen = paramsLen - argsLen

          partial =
            let (toBind, rest) = splitAt argsLen params
                env' = paramsMatch toBind args env in
            Closure (lambda { S.params = rest, S.paramsLen = restLen }) [] env'

          full =
            let (toBind, rest) = splitAt paramsLen args
                env' = paramsMatch params toBind env in
            case S.varParam lambda of
              Just _  -> Closure (lambda { S.params = [], S.paramsLen = 0 }) rest env'
              Nothing -> apply (evalExp' env' $ S.body lambda) rest
apply _ _ = applyError "not function type"

isFilled :: S.Lambda n -> Bool
isFilled lambda | S.params lambda == [] && S.paramsLen lambda == 0 = True
                | S.params lambda == []   = error "Inconsistent param."
                | S.paramsLen lambda == 0 = error "Inconsistent param."
                | otherwise = False

-- 可変引数関数の呼び出し
vcall :: Result n -> Result n
vcall (Closure lambda varArgs env)
  | Just var <- S.varParam lambda, isFilled lambda =
    evalExp' (Bind var (list varArgs) : env) (S.body lambda)
  | otherwise = error "Not variable param or not filled param closure is passed vcall!"
vcall _ =  error "Not closure is passwd vcall!"


evalLit :: S.Literal n -> Literal n
evalLit =  f
  where f (S.Num n) = Num n
        f (S.Str s) = Str s
        f (S.Quote _) = undefined

          
evalExp' :: Env n -> S.Exp n -> Result n
evalExp' env = eval
  where eval (S.Lit lit)  = Value $ Lit $ evalLit lit
        eval (S.EVar var) =
          maybe (unboundVariable var) bindValue
          $ find ((var ==) . bindKey) env
        eval (S.FApp fun args) =
          apply (eval fun) (map eval args)
        eval (S.VCall clo) =
          vcall $ eval clo
        eval (S.Abs lambda) = closure lambda env
        eval (S.Let binds expr) = evalExp' (letEnv env binds) expr

evalExp :: S.Exp n -> Result n
evalExp =  evalExp' []

qualifiedImport :: Env n -> S.Module n -> Env n
qualifiedImport env mod' =
  snd $ topEnv (S.name mod') env (S.binds mod')

run :: Env n -> S.Module n -> Result n
run env mod' =  evalExp' (qualifiedImport env mod') (S.EVar "Main.main")
