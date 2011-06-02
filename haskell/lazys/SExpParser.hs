
module SExpParser where

import Prelude hiding (concat)
import SExpSyntax (SExp)
import qualified SExpSyntax as Syntax

import Data.Char (toUpper)
import Text.ParserCombinators.ReadP
  (ReadP, readP_to_S,
   (+++), satisfy,
   get, char, string,
   skipSpaces, between, choice,
   sepBy, many1, many)
import Control.Monad (ap)
import Control.Applicative (Applicative(..),
                            (<$>), (<*>), (*>))

instance Applicative ReadP where
  pure  = return
  (<*>) = ap

choiceC :: String -> ReadP Char
choiceC =  choice . map char

empty :: ReadP [a]
empty =  return []

cons c cs = (:) <$> c <*> cs
(<:>) = cons

cons, (<:>) :: ReadP a -> ReadP [a] -> ReadP [a]

infixr 5 <:>

append a b = (++) <$> a <*> b
(<++>) = append

append, (<++>) :: ReadP [a] -> ReadP [a] -> ReadP [a]

infixr 5 <++>

preC :: Char -> ReadP String -> ReadP String
preC   c = (char c <:>)
prefix :: String -> ReadP String -> ReadP String
prefix s = (string s <++>)

concat :: [ReadP [a]] -> ReadP [a]
concat =  foldr (<++>) empty

spaceCharsP :: Char -> Bool
spaceCharsP =  (`elem` " \t\r\n")

trim :: ReadP a -> ReadP a
trim p = skipSpaces *> p <* skipSpaces

lParen = trim $ char '('
rParen = trim $ char ')'

lParen, rParen :: ReadP Char

list =  between lParen rParen exprList
exprList = sepBy expr skipSpaces

list, exprList :: (Num n, Read n) => ReadP [SExp n]

expr = (Syntax.list <$> list) +++ atom

atom = num +++ string' +++ symbol

expr, atom :: (Num n, Read n) => ReadP (SExp n)

{-# ANN escapeStrCharsP "HLint: ignore" #-}
escapeStrCharsP :: Char -> Bool
escapeStrCharsP =  (`elem` ['"', '\\'])

string' = between dquote dquote $ Syntax.string <$> many strChar

strChar =  normal +++ escaped
  where
    normal  = satisfy (not . escapeStrCharsP)
    escaped = bslash *> get

dquote = char '"'
bslash = char '\\'

strChar, dquote, bslash :: ReadP Char


escapeSymbolCharP :: Char -> Bool
escapeSymbolCharP =  (`elem` "()\"' \t\r\n")

symbol = Syntax.symbol <$> many1 symbolChar

symbolChar :: ReadP Char
symbolChar =  normal +++ escaped
  where normal = satisfy (not . escapeSymbolCharP)
        escaped = bslash *> (toUpper <$> get)

string', symbol :: (Num n, Read n) => ReadP (SExp n)

num :: (Num n, Read n) => ReadP (SExp n)
num =  float +++ int

float :: (Num n, Read n) => ReadP (SExp n)
float =  Syntax.readNum <$> (floatNormal +++
                             floatDotFirst +++
                             floatDotLast)

floatNormal   = decimal <++> dot <:> decimal
floatDotFirst = concat [empty, insertZeroS, dot <:> decimal]
floatDotLast  = concat [decimal, dotS, insertZeroS]

floatNormal, floatDotFirst, floatDotLast :: ReadP String

insertZero  = return '0'
insertZeroS = insertZero <:> empty

dot  = char '.'
dotS = dot <:> empty

insertZero, dot :: ReadP Char
insertZeroS, dotS :: ReadP String

int :: (Num n, Read n) => ReadP (SExp n)
int =  Syntax.readNum <$> (decimal +++
                           octal +++
                           hexadecimal)

decimal = many1 digit
octal       = (sharp *> insertZero) <:> choiceC "oO" <:> many1 octit
hexadecimal = (sharp *> insertZero) <:> choiceC "xX" <:> many1 hexit

decimal, octal, hexadecimal :: ReadP String

sharp = char '#'

digit = choiceC ['0'..'9']
octit = choiceC ['0'..'7']
hexit = digit +++ choiceC "abcdefABCDEF"
        
sharp, digit, octit, hexit :: ReadP Char

parseAtom :: (Num n, Read n) => ReadS (SExp n)
parseAtom = readP_to_S atom

parseExpr :: (Num n, Read n) => ReadS (SExp n)
parseExpr = readP_to_S expr

parseExprList :: (Num n, Read n) => ReadS [SExp n]
parseExprList = readP_to_S exprList

--
-- end of SExpParser.hs
--
