module MightParser where

import Data.Char

type Parser a = String -> [(a, String)]

returns :: a -> Parser a
returns v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
                       [] -> q inp
                       [(v, out)] -> [(v, out)]

(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>>= f = \inp -> case parse p inp of
                      [] -> []
                      [(v, out)] -> parse (f v) out

pp :: Parser (Char, Char)
pp = item >>>= \x ->
     item >>>= \_ ->
     item >>>= \y ->
     returns (x, y)

-- This wont work due to CaptB boodoo
--pp = do x <- item
        --item
        --y <- item
        --return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>>= \x ->
        if p x then returns x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = returns []
string (c:cs) = char c    >>>= \x  ->
                string cs >>>= \xs ->
                returns (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ returns []

many1 :: Parser a -> Parser [a]
many1 p = p      >>>= \v  ->
          many p >>>= \vs ->
          returns (v:vs)

ident :: Parser String
ident = lower         >>>= \x ->
        many alphanum >>>= \xs ->
        returns (x:xs)

nat :: Parser Int
nat = many1 digit >>>= \xs ->
      returns (read xs)

space :: Parser ()
space = many (sat isSpace) >>>= \_ ->
        returns ()

token :: Parser a -> Parser a
token p = space >>>= \_ ->
          p >>>= \v ->
          space >>>= \_ ->
          returns v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)


listParser :: Parser [Int]
listParser = symbol "[" >>>= \_ ->
             natural >>>= \n ->
             many (
                   symbol "," >>>= \_ ->
                   natural
                   ) >>>= \ns ->
             symbol "]" >>>= \_ ->
             returns (n:ns)

expr :: Parser Int
expr = term         >>>= \t ->
       (
        symbol "+"  >>>= \_ ->
        expr        >>>= \e ->
        returns (t + e)
        )
        +++ returns t

term :: Parser Int
term = factor       >>>= \f ->
       (
        symbol "*"  >>>= \_ ->
        term        >>>= \t ->
        returns (f * t)
        )
        +++ returns f

factor :: Parser Int
factor = (
         symbol "(" >>>= \_ ->
         expr       >>>= \e ->
         symbol ")" >>>= \_ ->
         returns e
          )
          +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
  [(n,[])]  -> n
  [(_,out)] -> error $ "unused input " ++ out
  []        -> error "invalid input"

--eval :: String -> Int
--eval xs = case parse expr xs of
               --[(n, [])] -> n
               --[(_, out)] -> error ("unconsumet input " ++ out)
               --[] -> error "invalid input"


