module SExpSyntax where

data Atom n = Num n
            | Str String
            | Id String
            --deriving Show

readNum' :: (Num n, Read n) => String -> Atom n
readNum' =  Num . read

readAtom :: (Num n, Read n) => String -> Atom n
readAtom xxs@(x:_) =
  case x of
    '"'  -> Str (read xxs :: String)
    _ | x `elem` ['0'..'9'] -> Num (read xxs)
    _    -> Id xxs
readAtom [] = error "Nul string input"

showAtom :: (Num n, Read n, Show n) => Atom n -> String
showAtom = f
  where f (Num n) = tag "num" $ show n
        f (Str str) = show str
        f (Id s)  = tag "symbol" s
        tag n s = '<':n ++  ':':s ++ ">"

instance (Show n, Read n, Num n) => Show (Atom n) where
  show = showAtom

data SExp' a = Atom a
             | Nil
             | (SExp' a) :! (SExp' a)
             deriving Show

infixr 5 :!

list :: [SExp' a] -> SExp' a
list = foldr (:!) Nil

type SExp n = SExp' (Atom n)

readNum :: (Num n, Read n) => String -> SExp n
readNum =  Atom . readNum'

string :: (Num n, Read n) => String -> SExp n
string =  Atom . Str

symbol :: (Num n, Read n) => String -> SExp n
symbol =  Atom . Id

quoteId :: SExp n
quoteId =  Atom (Id "quote")

quote :: SExp n -> SExp n
quote = (quoteId :!) . (:! Nil)
