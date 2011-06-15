module ParseResult (
  ParseResult,
  (<|>),
  errorResult,
  successResult
  ) where

import Control.Monad.Instances ()

type ParseResult exp = Either String exp

(<|>) :: ParseResult exp -> ParseResult exp -> ParseResult exp
(Left  _)   <|> b = b
a@(Right _) <|> _ = a

infixl 3 <|>

errorResult :: String -> ParseResult exp
errorResult =  Left

successResult :: exp -> ParseResult exp
successResult =  return
