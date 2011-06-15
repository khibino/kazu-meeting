module SExpSyntax (
  Atom(..), SExp'(..), SExp,
  fromList,
  fromList1,
  string, symbol,

  integer, double,
  readInteger, readDouble,
  quote,
  
  functionValue,

  ParseResult,
  toList, toList1) where

import Control.Arrow (first)
import ParseResult (ParseResult)
import PrimNum(PNum, fromDouble)

data Atom n = Num n
            | Str String
            | Id String
            deriving Eq
            --deriving Show

showAtom :: (Num n, Show n) => Atom n -> String
showAtom = f
  where f (Num n) = tag "num" $ show n
        f (Str str) = show str
        f (Id s)  = tag "symbol" s
        tag n s = '<':n ++  ':':s ++ ">"

instance (Show n, Num n) => Show (Atom n) where
  show = showAtom

data SExp' a = Atom a
             | (SExp' a) :! (SExp' a)
             | Nil
             deriving (Show, Eq)

infixr 5 :!

num :: a -> SExp' (Atom a)
num = Atom . Num

type SExp = SExp' (Atom PNum)


integer :: Integer -> SExp
integer = num . fromInteger

double :: Double -> SExp
double  = num . fromDouble

readInteger :: String -> SExp
readInteger =  integer . (read :: String -> Integer)

readDouble :: String -> SExp
readDouble =  double . (read :: String -> Double)

string :: String -> SExp
string =  Atom . Str

symbol :: String -> SExp
symbol =  Atom . Id

quoteId :: SExp
quoteId =  Atom (Id "quote")

quote :: SExp -> SExp
quote = (quoteId :!) . (:! Nil)

functionValue :: SExp -> Bool
functionValue =  pre
  where pre (Nil) = False
        pre (Atom (Num _)) = False
        pre (Atom (Str _)) = False
        pre (Atom (Id _))  = True
        pre (form :! _) = pre form

type List1 a = ([a], Maybe a)

fromList :: [SExp' a] -> SExp' a
fromList = foldr (:!) Nil

fromList1 :: List1 (SExp' a) -> SExp' a
fromList1 (list, last') = foldr (:!) (fromMaybe last') list
  where fromMaybe (Just atom) = atom
        fromMaybe Nothing     = Nil

toList1 :: SExp' a -> List1 (SExp' a)
toList1 = rec
  where rec (e:!es)       = first (e :) (rec es)
        rec Nil           = ([], Nothing)
        rec atom@(Atom _) = ([], Just atom)

toList :: SExp -> ParseResult [SExp]
toList exp' = 
  case toList1 exp' of
    (expL, Nothing) -> Right expL
    (   _, Just _)  -> Left  ("Not proper list: " ++ show exp')
