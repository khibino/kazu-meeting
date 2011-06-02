
module SExpParser where

import Prelude hiding (concat)
import SExpSyntax (SExp)
import qualified SExpSyntax as Syntax

import Data.Char (toUpper, isSpace)
import Text.ParserCombinators.ReadP
  (ReadP, readP_to_S, readS_to_P,
   (+++), satisfy, eof,
   get, char, string,
   optional,
   skipSpaces, between, choice,
   skipMany1, many1, many)
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

peek :: ReadP a -> ReadP ()
peek p = readS_to_P readS
  where readS input =
          case readP_to_S p input of
            _:_ -> [((), input)]
            []  -> []

skipSpaces1 :: ReadP ()
skipSpaces1 =  skipMany1 $ satisfy isSpace

trim :: ReadP a -> ReadP a
trim p = skipSpaces *> p <* skipSpaces

lParen = char '('
rParen = char ')'

lParen, rParen :: ReadP Char

quote =  char '\''
dQuote = char '"'
bslash = char '\\'

quote, dQuote, bslash :: ReadP Char


list =  between lParen rParen exprList
exprList = many expr

list, exprList :: ReadP [SExp]

expr = trim ((Syntax.list <$> list) +++ atom ) +++
       (skipSpaces *> (Syntax.quote <$> (quote *> expr)))

atom = num +++ string' +++ symbol

expr, atom :: ReadP SExp

{-# ANN escapeStrCharsP "HLint: ignore" #-}
escapeStrCharsP :: Char -> Bool
escapeStrCharsP =  (`elem` ['"', '\\'])

string' = between dQuote dQuote $ Syntax.string <$> many strChar

strChar :: ReadP Char
strChar =  normal +++ escaped
  where
    normal  = satisfy (not . escapeStrCharsP)
    escaped = bslash *> get


escapeSymbolCharP :: Char -> Bool
escapeSymbolCharP =  (`elem` (['0'..'9'] ++ "()#\"' \t\r\n"))

tokenSep :: ReadP ()
tokenSep =  skipSpaces1 +++
            peek (lParen +++ rParen +++
                  dQuote +++ quote) +++
            peek eof

symbol = Syntax.symbol <$> many1 symbolChar <* tokenSep

symbolChar :: ReadP Char
symbolChar =  normal +++ escaped
  where normal = satisfy (not . escapeSymbolCharP)
        escaped = bslash *> (toUpper <$> get)

string', symbol :: ReadP SExp

num :: ReadP SExp
num =  (float +++ int) <* tokenSep

float :: ReadP SExp
float =  Syntax.readDouble <$> (floatNormal +++
                                floatDotFirst)

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

int :: ReadP SExp
int =  Syntax.readInteger <$> ((decimal <* optional dot) +++
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

parseFloat :: ReadS SExp
parseFloat =  readP_to_S float

parseAtom :: ReadS SExp
parseAtom =  readP_to_S atom

parseExpr :: ReadS SExp
parseExpr =  readP_to_S expr

parseExprList :: ReadS [SExp]
parseExprList =  readP_to_S (exprList <* eof)

test0 :: [(SExp, String)]
test0 =  parseExpr "(let ((x 1)) x)"

--
-- end of SExpParser.hs
--
