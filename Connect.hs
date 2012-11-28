{--
 -- Natume -- an implementation of Kana-Kanji conversion in Haskell
 -- Copyright (C) 2006-2012 Takayuki Usui
 --
 -- This program is free software; you can redistribute it and/or modify
 -- it under the terms of the GNU General Public License as published by
 -- the Free Software Foundation; either version 2 of the License, or
 -- (at your option) any later version.
 --
 -- This program is distributed in the hope that it will be useful,
 -- but WITHOUT ANY WARRANTY; without even the implied warranty of
 -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -- GNU General Public License for more details.
 --
 -- You should have received a copy of the GNU General Public License
 -- along with this program; if not, write to the Free Software
 -- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 --}
module Connect (
    Rule,Connect,
    readconn
  ) where

import System.IO
import ScmParse
import ScmToken

type Rule = [([String],String,String,String)]
type Connect = (Rule,Int)

eval3 :: [Expr] -> [String]
eval3 []     = []
eval3 (x:xs) = f : eval3 xs where (ExprName f) = x

eval2 :: [Expr] -> Rule
eval2 []     = []
eval2 (x:xs) =
  case x of
    (ExprList [ExprList [ExprList h]]) ->
      (eval3 h,[],[],[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName "*"]]) ->
      (eval3 h,[],[],[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName "*",ExprName "*"]]) ->
      (eval3 h,[],[],[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName t]]) ->
      (eval3 h,t,[],[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName t,ExprName "*"]]) ->
      (eval3 h,t,[],[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName "*",ExprName f]]) ->
      (eval3 h,[],f,[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName t, ExprName f]]) ->
      (eval3 h,t,f,[]) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName "*",ExprName "*",ExprName g]]) ->
      (eval3 h,[],[],g) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName t,ExprName "*",ExprName g]]) ->
      (eval3 h,t,[],g) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName "*",ExprName f,ExprName g]]) ->
      (eval3 h,[],f,g) : (eval2 xs)
    (ExprList [ExprList [ExprList h,ExprName t,ExprName f,ExprName g]]) ->
      (eval3 h,t,f,g) : (eval2 xs)
    _ -> error "Connect.eval2: illegal rule"
      
eval1 :: Expr -> Connect
eval1 x =
  case x of
    ExprList [ExprList rule,ExprInt cost] ->
      (eval2 rule,cost)
    _ -> error "Connect.eval1: illegal rule"

eval :: [Expr] -> [Connect]
eval []     = []
eval (x:xs) = (eval1 x) : eval xs

readconn :: String -> IO [Connect]
readconn path = do h <- openFile path ReadMode
                   hSetEncoding h latin1
                   s <- hGetContents h
                   return (eval (parse (tokenize s)))

{-
test :: String -> IO ()
test path =
  do cs <- readconn path
     mapM (\(ys,cost) ->
              do mapM (\(ns,_,_,_) ->
                          do putStr (foldl1 (\x y -> x ++ "-" ++ y) ns)
                             putStr ",")
                      ys
                 putStrLn (show cost))
          cs
     putStrLn (show (foldl (\x (_,y) -> x + y) 0 cs))
     return ()
-}
