
module Evaluator where

import Data.List (find)
import PrimNum (PNum)
import qualified Syntax

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
              | Closure { lambdaCode :: Syntax.Lambda' n
                        , closureEnv :: Env n
                        , closureVarArgs :: [Result n]
                        }
              | Primitive { primCode  :: Result n -> Result n }

instance Show n => Show (Result n) where
  show (Value v) = show v
  show (IO _)    = "<IO>"
  show (Closure _ _ _) = "<Closure>"
  show (Primitive _) = "<Primitive>"

prim2 :: (Result n -> Result n -> Result n) -> Result n
prim2 f = Primitive (Primitive . f)

getNum :: Num n => Result n -> n
getNum (Value (Lit (Num n))) = n
getNum  v = error ("not number: " ++ show v)

op2num :: Num n => (n -> n -> n) -> Result n
op2num op = prim2 fun
  where fun x y = Value $ Lit $ Num $ getNum x `op` getNum y

true  = closure (Syntax.lambda (map Syntax.PVar [ "t", "f"]) Nothing (Syntax.EVar "t")) []
false = closure (Syntax.lambda (map Syntax.PVar [ "t", "f"]) Nothing (Syntax.EVar "f")) []

if' = closure (Syntax.lambda (map Syntax.PVar ["l", "m", "n"]) Nothing
               (Syntax.FApp (Syntax.EVar "l") (map Syntax.EVar ["m", "n"]))) []

true, false, if' :: Result n

op2bool :: Num n => (n -> n -> Bool) -> Result n
op2bool op = prim2 fun
  where fun x y = if getNum x `op` getNum y then true else false



list :: [Result n] -> Result n
list =  Value . Lit . List

data Bind n = Bind { bindKey   :: Var
                   , bindValue :: Result n
                   }

type Env n = [Bind n]


primitives :: Env PNum
primitives =
  map (uncurry Bind)
  [("+", op2num (+)),
   ("-", op2num (-)),
   ("*", op2num (*)),
   ("/", op2num (/)),
   
   ("if", if'),
   ("true", true),
   ("false", false),
   
   ("=", op2bool (==)),
   (">=", op2bool (>=)),
   ("<=", op2bool (<=)),
   (">", op2bool (>)),
   ("<", op2bool (<))
   ]


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

patternMatch :: Syntax.Pat -> Result n -> [(Var, Result n)]
patternMatch pat value = dispatch pat
  where dispatch (Syntax.PVar var) = [(var, value)]

paramsMatch :: [Syntax.Pat] -> [Result n] -> Env n -> Env n
paramsMatch pats values env =
  extendEnv env
  $ concatMap (uncurry patternMatch) (zip pats values)

closure :: Syntax.Lambda' n -> Env n -> Result n
closure lambda env = Closure { lambdaCode = lambda
                             , closureEnv = env
                             , closureVarArgs = []
                             }

bindMatch :: Env n -> Syntax.Bind' n -> [(Var, Result n)]
bindMatch env = match
  where match (Syntax.BPat pat expr')  = patternMatch pat (evalExp' env expr')
        match (Syntax.BFun var lambda) = [(var, closure lambda env)]

letEnv :: Env n -> [Syntax.Bind' n] -> Env n
letEnv env binds = recEnv'
  where recEnv' = extendEnv env $ concatMap match binds
        match = bindMatch recEnv'

topEnv :: Mod -> Env n -> [Syntax.Bind' n] -> (Env n, Env n)
topEnv qual env binds = envPair
  where envPair@(recEnv', _) = extendEnvTop qual env $ concatMap match binds
        match = bindMatch recEnv'

applyError :: String -> a
applyError =  evalError "apply"

unboundVariable :: String -> a
unboundVariable =  applyError . (++ ": binding not found!")

apply :: Result n -> [Result n] -> Result n
apply fun                      []   = fun
apply (Closure lambda env _) args =
  if   restLen > 0 then partial
  else full
    where argsLen = length args
          params = Syntax.params lambda
          paramsLen = Syntax.paramsLen lambda
          restLen = paramsLen - argsLen

          partial =
            let (toBind, rest) = splitAt argsLen params
                env' = paramsMatch toBind args env in
            Closure { lambdaCode = lambda { Syntax.params = rest, Syntax.paramsLen = restLen }
                    , closureEnv = env'
                    , closureVarArgs = []
                    }

          full =
            let (toBind, rest) = splitAt paramsLen args
                env' = paramsMatch params toBind env in
            case Syntax.varParam lambda of
              Just _  -> Closure { lambdaCode = lambda { Syntax.params = [], Syntax.paramsLen = 0 }
                                 , closureEnv = env'
                                 , closureVarArgs = rest
                                 }
              Nothing -> apply (evalExp' env' $ Syntax.body lambda) rest
apply (Primitive code) (a:as)  = apply (code a) as
apply  _                     _ = applyError "not function value"

isFilled :: Syntax.Lambda' n -> Bool
isFilled lambda | Syntax.params lambda == [] && Syntax.paramsLen lambda == 0 = True
                | Syntax.params lambda == []   = error "Inconsistent param."
                | Syntax.paramsLen lambda == 0 = error "Inconsistent param."
                | otherwise = False

-- 可変引数関数の呼び出し
vcall :: Result n -> Result n
vcall (Closure lambda env varArgs)
  | Just var <- Syntax.varParam lambda, isFilled lambda =
    evalExp' (Bind var (list varArgs) : env) (Syntax.body lambda)
  | otherwise = error "Not variable param or not filled param closure is passed vcall!"
vcall (Primitive  _) = error "Not implemeted. - primitive vcall"
vcall _ =  error "Not closure is passed vcall!"


evalLit :: Syntax.Literal' n -> Literal n
evalLit =  f
  where f (Syntax.Num n) = Num n
        f (Syntax.Str s) = Str s
        f (Syntax.SExp _) = undefined

          
evalExp' :: Env n -> Syntax.Exp' n -> Result n
evalExp' env = eval
  where eval (Syntax.Lit lit)  = Value $ Lit $ evalLit lit
        eval (Syntax.EVar var) =
          maybe (unboundVariable var) bindValue
          $ find ((var ==) . bindKey) env
        eval (Syntax.FApp fun args) =
          apply (eval fun) (map eval args)
        eval (Syntax.VCall clo) =
          vcall $ eval clo
        eval (Syntax.Abs lambda) = closure lambda env
        eval (Syntax.Let binds expr) = evalExp' (letEnv env binds) expr

evalExp :: Syntax.Exp' PNum -> Result PNum
evalExp =  evalExp' primitives

qualifiedImport :: Env n -> Syntax.Module' n -> Env n
qualifiedImport env mod' =
  snd $ topEnv (Syntax.name mod') env (Syntax.binds mod')

run :: Env PNum -> Syntax.Module' PNum -> Result PNum
run env mod' =  evalExp' (primitives ++ qualifiedImport env mod') (Syntax.EVar "Main.main")
