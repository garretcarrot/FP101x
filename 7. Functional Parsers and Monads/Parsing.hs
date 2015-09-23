module Parsing where

import Data.Char
import Control.Monad
infixr 5 +++

-- The monad of parsers
-----------------------

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
  return v                   =  P (\inp -> [(v,inp)])
  p >>= f                    =  P (\inp -> case parse p inp of
                                              []        -> []
                                              [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero                      =  P (\inp -> [])
  p `mplus` q                =  P (\inp -> case parse p inp of
                                              []        -> parse q inp
                                              [(v,out)] -> [(v,out)])

-- Basic parsers
----------------

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

-- Choice
---------

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

-- Derived primitives
---------------------

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

-- pointfree style
char' :: Char -> Parser Char
char' = sat . (==)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)
 
ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)
 
nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  (do char '-'
                                     n <- nat
                                     return (- n)
                                 ) +++ nat
 
space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

comment                       :: Parser ()
comment                       =  do string "--"
                                    many (sat (/= '\n'))
                                    -- char '\n'
                                    return ()

expr'                          :: Parser Int
expr'                          = do { n <- natural;
                                      ns <- many
                                            (do {symbol "-";
                                                natural});
                                      return (foldl (-) n ns) }

-- Ignoring spacing
-------------------

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int
symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

-- expr   ::= term ('+' expr | '−' expr | ε) 
-- term   ::= expnt ('∗' term | '/' term | ε)
-- expnt  ::= factor ('^' expnt | ε)
-- factor ::= '(' expr ')' | nat
-- nat    ::= 0 | 1 | 2 | ···

expr :: Parser Int
expr = 
  do {
    t <- term;
    do {
      symbol "+";
      e <- expr;
      return (t + e)
    } +++ do {
      symbol "-";
      e <- expr;
      return (t - e)
    } +++ return t
  }

term :: Parser Int
term =
  do {ex <- expnt;
      do {symbol "*";
          t <- term;
          return (ex * t)} +++
      do {symbol "/";
          t <- term;
          return (ex `div` t)} +++ 
      return ex}

expnt :: Parser Int
expnt = 
  do {f <- factor;
      do {symbol "^";
          ex <- expnt;
          return (f ^ ex)} +++ return f}

factor :: Parser Int
factor =
  do {symbol "(";
        e <- expr;
        symbol ")";
        return e}
    +++ natural

-- Exercise 8.
-- sub ::= nat rest
-- minus ::= ("-" nat)
-- rest ::= minus*

minus :: Parser Int
minus = do symbol "-"
           natural

rest :: Parser [Int]
rest = many minus

sub :: Parser Int
sub = do n <- natural
         ns <- rest
         return $ foldl (-) n ns