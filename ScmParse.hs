{--
 --  Natume -- an implementation of Kana-Kanji conversion in Haskell
 --  Copyright (C) 2006-2012 Takayuki Usui
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
module ScmParse (
    Expr(ExprInt,
         ExprName,
         ExprString,
         ExprList,
         ExprAlt,
         ExprAnno),
    parse,pp
  ) where

import Parser
import ScmToken

data Expr = ExprInt Int
          | ExprName String
          | ExprString String
          | ExprList [Expr]
          | ExprAlt [Expr]
          | ExprAnno Expr
          deriving (Show)

immediate :: Parser Token Expr
immediate = MkParser f
            where
            f []                   = []
            f ((TokenInt i):ts)    = [(ExprInt i,ts)]
            f ((TokenName s):ts)   = [(ExprName s,ts)]
            f ((TokenString s):ts) = [(ExprString s,ts)]
            f (_:_)                = []

leftparen  :: Parser Token ()
leftparen  = MkParser f
             where
             f []                    = []
             f ((TokenLeftParen):ts) = [((),ts)]
             f (_:_)                 = []

rightparen :: Parser Token ()
rightparen = MkParser f
             where
             f []                     = []
             f ((TokenRightParen):ts) = [((),ts)]
             f (_:_)                  = []

leftbrace  :: Parser Token ()
leftbrace  = MkParser f
             where
             f []                    = []
             f ((TokenLeftBrace):ts) = [((),ts)]
             f (_:_)                 = []

rightbrace :: Parser Token ()
rightbrace = MkParser f
             where
             f []                     = []
             f ((TokenRightBrace):ts) = [((),ts)]
             f (_:_)                  = []

leftbrace' :: Parser Token Expr
leftbrace' = MkParser f
             where
             f []                    = []
             f ((TokenLeftBrace):ts) = [(ExprName "{",ts)]
             f (_:_)                 = []

rightbrace' :: Parser Token Expr
rightbrace' = MkParser f
              where
              f []                    = []
              f ((TokenRightBrace):ts) = [(ExprName "}",ts)]
              f (_:_)                 = []

leftbracket  :: Parser Token ()
leftbracket  = MkParser f
               where
               f []                      = []
               f ((TokenLeftBracket):ts) = [((),ts)]
               f (_:_)                   = []

rightbracket :: Parser Token ()
rightbracket = MkParser f
               where
               f []                       = []
               f ((TokenRightBracket):ts) = [((),ts)]
               f (_:_)                    = []

leftbracket'  :: Parser Token Expr
leftbracket'  = MkParser f
                where
                f []                      = []
                f ((TokenLeftBracket):ts) = [(ExprName "[",ts)]
                f (_:_)                   = []

rightbracket' :: Parser Token Expr
rightbracket' = MkParser f
                where
                f []                       = []
                f ((TokenRightBracket):ts) = [(ExprName "]",ts)]
                f (_:_)                    = []

slash :: Parser Token ()
slash = MkParser f
        where
        f []                   = []
        f ((TokenName "/"):ts) = [((),ts)]
        f (_:_)                = []

alternate :: Parser Token Expr
alternate = do leftbrace
               e <- immediate
               es <- more
               return (ExprAlt (e:es))
            where
            more = do slash
                      e <- immediate
                      es <- more
                      return (e:es)
                   `orelse`
                   do slash
                      rightbrace
                      return []
                   `orelse`
                   do rightbrace
                      return []

annotation :: Parser Token Expr
annotation = do leftbracket
                e <- expression
                rightbracket
                return (ExprAnno e)

list :: Parser Token Expr
list = do leftparen
          es <- many expression
          rightparen
          return (ExprList es)

expression :: Parser Token Expr
expression = immediate `orelse`
             list `orelse`
             alternate `orelse`
             annotation `orelse`
             leftbrace' `orelse`
             rightbrace' `orelse`
             leftbracket' `orelse`
             rightbracket'

parse1 :: [Token] -> ([Expr],[Token])
parse1 ts = case (applyParser expression ts) of
              []        -> ([],ts)
              ((e,r):_) -> ([e],r)

parse :: [Token] -> [Expr]
parse ts = let (es,r) = (parse1 ts) in
           if null es
           then if not (null r)
                then error ("parse error -- " ++ show (take 3 r) ++ "...")
                else []
           else (head es) : (parse r)

indent :: Int -> String
indent d = replicate d ' '

width :: String -> Int
width f = foldl (max) 0 (map length (lines f))

fitin :: String -> Bool
fitin p = width p < 72

allow :: String -> Bool
allow f = length (lines f) == 1

recal :: Int -> String -> Int
recal n f = if allow f then n + w else w
            where w = width f
            
fmt3 :: Int -> Int -> [Expr] -> [String]
fmt3 _ _ []     = [")"]
fmt3 d n [x]    = [f1 ++ ")" | f1 <- fmt1 (d+1) n x]
fmt3 d n (x:xs) = [f1 ++ "\n" ++ indent n ++ f3 |
                   f1 <- if d < 2 then fmt1 (d+1) n x
                                  else fmt3' (d+1) n x,
                   f3 <- fmt3 (d+1) n xs]

fmt3' :: Int -> Int -> Expr -> [String]
fmt3' _ _ (ExprName s)   = [s]
fmt3' _ _ (ExprInt i)    = [show i]
fmt3' _ _ (ExprString s) = [show s]
fmt3' d n (ExprList es)  = ["(" ++ f3 | f3 <- fmt3 (d+1) (n+1) es]
fmt3' _ _ _              = error "ScmParse.fmt3'"

fmt2 :: Int -> Int -> [Expr] -> [String]
fmt2 _ _ []     = [")"]
fmt2 d n [x]    = [f1 ++ ")"       | f1 <- fmt1 (d+1) n x]
fmt2 d n (x:xs) = [f1 ++ " " ++ f2 |
                   f1 <- if d < 3 then fmt1 (d+1) n x
                                  else fmt2' (d+1) n x,
                   f2 <- fmt2 (d+1) ((recal n f1) + 1) xs]

fmt2' :: Int -> Int -> Expr -> [String]
fmt2' _ _ (ExprName s)   = [s]
fmt2' _ _ (ExprInt i)    = [show i]
fmt2' _ _ (ExprString s) = [show s]
fmt2' d n (ExprList es)  = ["(" ++ f2 | f2 <- fmt2 (d+1) (n+1) es]
fmt2' _ _ _              = error "ScmParse.fmt2'"

fmt1 :: Int -> Int -> Expr -> [String]
fmt1 _ _ (ExprName s)   = [s]
fmt1 _ _ (ExprInt i)    = [show i]
fmt1 _ _ (ExprString s) = [show s]
fmt1 d n (ExprList es)  = ["(" ++ f2 | f2 <- fmt2 (d+1) (n+1) es] ++
                          ["(" ++ f3 | f3 <- fmt3 (d+1) (n+1) es]
fmt1 d n (ExprAlt  es)  = fmt1 d n (ExprList es)
fmt1 _ _ _              = error "ScmParse.fmt1"

fmt0 :: Expr -> [String]
fmt0 e = [f1 | f1 <- fmt1 0 0 e]

choice :: [String] -> String
choice ss = head opts
            where opts = [p | p <- filter fitin ss]

prettyShow :: Expr -> String
prettyShow e = choice (fmt0 e)

prettyPrint :: Expr -> IO ()
prettyPrint e = do putStrLn (prettyShow e); return ()

pp :: Expr -> IO ()
pp = prettyPrint

{-
test = do h <- openFile "test.scm" ReadMode
          hSetEncoding h latin1
          s <- hGetContents h
          let es = parse (tokenize s)
          mapM pp es
          putStrLn ("total=" ++ (show (length es)))
          return ()
-}
