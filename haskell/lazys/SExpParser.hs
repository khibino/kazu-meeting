
module SExpParser (
  parseFloat,
  parseAtom,
  parseExpr,
  parseExprList
  ) where

import Prelude hiding (concat)
import ParseResult (ParseResult, successResult, errorResult)
import SExpSyntax (SExp)
import qualified SExpSyntax as Syntax

import Data.Char (toUpper, isSpace)
import Text.ParserCombinators.ReadP
  (ReadP, readP_to_S, readS_to_P,
   (+++), satisfy, eof,
   get, char,
   option,
   optional,
   between, choice,
   skipMany1, skipMany,
   many1, many)
import Control.Monad (ap)
import Control.Applicative (Applicative(..),
                            (<$>), (<*>), (*>))

instance Applicative ReadP where
  pure  = return
  (<*>) = ap

toReadP :: a -> ReadP a
toReadP =  return

choiceC :: String -> ReadP Char
choiceC =  choice . map char

empty :: ReadP [a]
empty =  toReadP []

cons c cs = (:) <$> c <*> cs
(<:>) = cons

cons, (<:>) :: ReadP a -> ReadP [a] -> ReadP [a]

infixr 5 <:>

append a b = (++) <$> a <*> b
(<++>) = append

append, (<++>) :: ReadP [a] -> ReadP [a] -> ReadP [a]

infixr 5 <++>

concat :: [ReadP [a]] -> ReadP [a]
concat =  foldr (<++>) empty

peek :: ReadP a -> ReadP ()
peek p = readS_to_P readS
  where readS input =
          case readP_to_S p input of
            _:_ -> [((), input)]
            []  -> []

satisfy_ :: (Char -> Bool) -> ReadP ()
satisfy_ =  (*> toReadP ()) . satisfy

char_ :: Char -> ReadP ()
char_ =  (*> toReadP ()) . char

semi :: ReadP Char
semi =  char ';'

comment :: ReadP ()
comment =  semi *> skipMany (satisfy (/= '\n')) *> char_ '\n' 

blank :: ReadP ()
blank =  satisfy_ isSpace +++ comment

skipBlanks1 :: ReadP ()
skipBlanks1 =  skipMany1 blank

skipBlanks :: ReadP ()
skipBlanks =  skipMany blank

trimL :: ReadP a -> ReadP a
trimL p = skipBlanks *> p

lParen = trimL $ char '('
rParen = trimL $ char ')'

lParen, rParen :: ReadP Char

quote =  char '\''
dQuote = char '"'
bslash = char '\\'

quote, dQuote, bslash :: ReadP Char


list :: ReadP SExp
list =  Syntax.fromList1 <$>
        between lParen rParen
        ((,) <$> exprList <*> option Nothing (Just <$> (dot *> expr)))

exprList :: ReadP [SExp]
exprList = many expr


expr = list +++ atom +++
       (Syntax.quote <$> trimL (quote *> expr))

atom = trimL $ num +++ string' +++ symbol

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
tokenSep =  peek (skipBlanks1 +++
                  (lParen +++ rParen +++
                   dQuote +++ quote *> return ()) +++
                  eof)

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
--最後が dot のときは整数に解釈
--floatDotLast  = concat [decimal, dotS, insertZeroS]

floatNormal, floatDotFirst :: ReadP String

insertZero  = return '0'
insertZeroS = insertZero <:> empty

dot  = char '.'

insertZero, dot :: ReadP Char
insertZeroS :: ReadP String

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

eof' :: ReadP ()
eof' =  trimL eof

readPtoParser :: Show a => ReadP a -> String -> ParseResult a
readPtoParser p = result "S-expression parse error!" . readP_to_S p
  where result _ ((rv, ""):_)   = successResult rv
        result s ((rv, rest):_) = errorResult (s ++ "\nrest input: " ++ rest
                                               ++ show rv)
        result s []             = errorResult s

parseFloat :: ReadS SExp
parseFloat =  readP_to_S float

parseAtom :: ReadS SExp
parseAtom =  readP_to_S atom

parseExpr :: String -> ParseResult SExp
parseExpr =  readPtoParser (expr <* eof')

parseExprList :: String -> ParseResult [SExp]
parseExprList =  readPtoParser (exprList <* eof')


-- for debug

-- parseList0 = readP_to_S list
-- t0 = parseList0 "(x)"

-- parseList1 = readP_to_S $ between lParen rParen exprList
-- t1 = parseList1 "(x)"

-- parseList2 = readP_to_S $ Syntax.fromList1 <$>
--              between lParen rParen ((,) [] <$> option Nothing (Just <$> expr))
-- t2 = parseList2 "(x)"

--
-- end of SExpParser.hs
--
