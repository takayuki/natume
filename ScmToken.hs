{--
 --  Natume -- an implementation of Kana-Kanji conversion in Haskell
 --  Copyright (C) 2006-2011 Takayuki Usui
 --
 --  This program is free software; you can redistribute it and/or modify
 --  it under the terms of the GNU General Public License as published by
 --  the Free Software Foundation; either version 2 of the License, or
 --  (at your option) any later version.
 --
 --  This program is distributed in the hope that it will be useful,
 --  but WITHOUT ANY WARRANTY; without even the implied warranty of
 --  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 --  GNU General Public License for more details.
 --
 --  You should have received a copy of the GNU General Public License
 --  along with this program; if not, write to the Free Software
 --  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 --}
module ScmToken (
    Token(TokenLeftParen,
          TokenRightParen,
          TokenLeftBrace,
          TokenRightBrace,
          TokenLeftBracket,
          TokenRightBracket,
          TokenInt,
          TokenName,
          TokenString),
    tokenize
  ) where

import Data.Char
import Parser

data Token = TokenLeftParen
           | TokenRightParen
           | TokenLeftBrace
           | TokenRightBrace
           | TokenLeftBracket
           | TokenRightBracket
           | TokenInt Int
           | TokenName String
           | TokenString String
           | TokenIgnore

instance Show Token where
    show (TokenLeftParen)    = "("
    show (TokenRightParen)   = ")" 
    show (TokenLeftBrace)    = "{"
    show (TokenRightBrace)   = "}" 
    show (TokenLeftBracket)  = "["
    show (TokenRightBracket) = "]" 
    show (TokenInt i)        = show i
    show (TokenName s)       = s
    show (TokenString s)     = show s
    show (TokenIgnore)       = ""

item :: Parser Char Char
item = MkParser f 
       where f []     = []
             f (x:xs) = [(x,xs)]

sat :: (Char -> Bool) -> Parser Char Char
sat test = MkParser f
           where f []     = []
                 f (x:xs) | test x    = [(x,xs)]
                          | otherwise = []

esat :: (Char -> Bool) -> Parser Char Char
esat test = MkParser f
            where f []     = []
                  f (x:xs) | x == '\\' = applyParser (sat test) xs
                           | test x    = [(x,xs)]
                           | otherwise = []

leftparen :: Parser Char Token
leftparen = do sat (=='('); return TokenLeftParen

rightparen :: Parser Char Token
rightparen = do sat (==')'); return TokenRightParen

leftbrace :: Parser Char Token
leftbrace = do sat (=='{'); return TokenLeftBrace

rightbrace :: Parser Char Token
rightbrace = do sat (=='}'); return TokenRightBrace

leftbracket :: Parser Char Token
leftbracket = do sat (=='['); return TokenLeftBracket

rightbracket :: Parser Char Token
rightbracket = do sat (==']'); return TokenRightBracket

digit :: Parser Char Int
digit = do x <- sat isDigit; return (ord x - ord '0')

nat :: Parser Char Int
nat = do xs <- some digit
         let n = foldl1 (\x y -> x * 10 + y) xs
         return n

sign :: Parser Char Char
sign = sat (\x -> x == '+' || x == '-')

signed :: Parser Char Int
signed = do x <- sign; n <- nat
            case x of
               '-' -> return (negate n)
               _   -> return n


unsigned :: Parser Char Int
unsigned = do n <- nat; return n

integer :: Parser Char Token
integer = do n <- signed `orelse` unsigned; return (TokenInt n)

backslash :: Parser Char Char
backslash = sat (=='\\')

escape :: Parser Char Char
escape = do backslash; c <- item; return c

char :: Parser Char Char
char = sat (\x -> x /= '"' && x /= '\\')

nonescape :: Parser Char Char
nonescape = do c <- char; return c

doublequote :: Parser Char Char
doublequote = sat (=='"')

string :: Parser Char Token
string = do doublequote
            s <- many (escape `orelse` nonescape)
            doublequote
            return (TokenString s)

alpha1 :: Parser Char String
alpha1 = do b0 <- sat (\x -> isalpha x)
            return [b0]


alphanum1 :: Parser Char String
alphanum1 = do b0 <- sat (\x -> isalpha x || isDigit x || any (x==) "-")
               return [b0]

multibyte2 :: Parser Char String
multibyte2 = do b0 <- sat (\x -> ('\xA1' <= x && x <= '\xFE') || x == '\x8E')
                b1 <- item
                return [b0,b1]

multibyte3 :: Parser Char String
multibyte3 = do b0 <- sat (\x -> x == '\x8F')
                b1 <- item
                b2 <- item
                return [b0,b1,b2]

isalpha :: Char -> Bool
isalpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

alpha :: Parser Char Char
alpha = esat (\x -> isalpha x || any (x==) "!\"#$%&'*+,-./:;<>=?@\\^_`|~")

alphanum :: Parser Char Char
alphanum = alpha `orelse` esat isDigit

identifier :: Parser Char Token
identifier = do x <- alpha1 `orelse` multibyte2 `orelse` multibyte3
                xs <- many (alphanum1 `orelse` multibyte2 `orelse` multibyte3)
                return (TokenName (x ++ concat xs))
             `orelse`
             do x <- alpha
                xs <- many alphanum
                return (TokenName (x:xs))

semicolon ::  Parser Char Char
semicolon = sat (==';')

nonnewline ::  Parser Char Char
nonnewline = sat (/='\n')

comment :: Parser Char Token
comment = do semicolon
             many nonnewline
             return (TokenIgnore)

token1 :: Parser Char Token
token1 =  comment      `orelse`
          leftparen    `orelse`
          rightparen   `orelse`
          leftbrace    `orelse`
          rightbrace   `orelse`
          leftbracket  `orelse`
          rightbracket `orelse`
          integer      `orelse`
          string       `orelse`
          identifier


space :: Parser Char Char
space = sat isSpace

token :: Parser Char Token
token = do many space
           t <- token1
           many space
           return t

tokenize1 :: String -> ([Token],String)
tokenize1 s = case (applyParser token s) of
                []        -> ([],s)
                ((t,r):_) -> case t of
                               (TokenIgnore) -> tokenize1 r
                               _             -> ([t],r)

tokenize :: String -> [Token]
tokenize s = let (ts,r) = (tokenize1 s) in
             if null ts
             then if not (null r)
                  then error ("lexcal error -- " ++ take 8 r ++ "...")
                  else []
             else (head ts) : (tokenize r)

