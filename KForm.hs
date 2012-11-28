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
module KForm (
    KForm(MkKForm),
    index,name,gobi,ygobi,pgobi,
    readform,compile,
    pp,fmt0,fmt1
  ) where

import System.IO
import ScmParse hiding (pp)
import ScmToken

data KForm = MkKForm { index  :: Int,
                       name   :: String,
                       gobi   :: String,
                       ygobi  :: String,
                       pgobi  :: String }
             deriving (Show)

eval2 :: [Expr] -> [KForm]
eval2 []     = []
eval2 (x:xs) =
  case x of
    (ExprList [ExprName f,ExprName "*"]) ->
      (MkKForm 0 f "" "" "") : (eval2 xs)
    (ExprList [ExprName f,ExprName "*",ExprName "*"]) ->
      (MkKForm 0 f "" "" "") : (eval2 xs)
    (ExprList [ExprName f,ExprName "*",ExprName "*",ExprName "*"]) ->
      (MkKForm 0 f "" "" "") : (eval2 xs)
    (ExprList [ExprName f,ExprName g]) ->
      (MkKForm 0 f g g g) : (eval2 xs)
    (ExprList [ExprName f,ExprName g,ExprName "*"]) ->
      (MkKForm 0 f g g g) : (eval2 xs)
    (ExprList [ExprName f,ExprName g,ExprName "*",ExprName "*"]) ->
      (MkKForm 0 f g g g) : (eval2 xs)
    (ExprList [ExprName f,ExprName g,ExprName y]) ->
      (MkKForm 0 f g y y) : (eval2 xs)
    (ExprList [ExprName f,ExprName g,ExprName y,ExprName "*"]) ->
      (MkKForm 0 f g y y) : (eval2 xs)
    (ExprList [ExprName f,ExprName g,ExprName y,ExprName p]) ->
      (MkKForm 0 f g y p) : (eval2 xs)
    _ ->
      error ("KForm.eval2: " ++ (show x))

eval1 :: Expr -> (String,[KForm])
eval1 x = (kname,eval2 kforms)
          where
          ExprList [ExprName kname,ExprList kforms] = x

eval :: [Expr] -> [(String,[KForm])]
eval []     = []
eval (x:xs) = (eval1 x) : eval xs

enum1 :: Int -> [KForm] -> (Int,[KForm])
enum1 n []     = (n,[])
enum1 n (x:xs) = (n1,(x {index=n}) : xs1)
                 where
                 (n1,xs1) = enum1 (n+1) xs

enum0 :: Int -> [[KForm]] -> [[KForm]]
enum0 _ []     = []
enum0 n (x:xs) = fs1 : (enum0 1 xs)
                 where
                 (_,fs1) = enum1 n x

enum :: [[KForm]] -> [[KForm]]
enum xs = enum0 1 xs

readform :: String -> IO [((Int,String),[KForm])]
readform path = do h <- openFile path ReadMode
                   hSetEncoding h latin1
                   s <- hGetContents h
                   let fs = (eval (parse (tokenize s)))
                   let (names,forms) = unzip fs
                   return (zip (zip [1..] names) (enum forms))

compile :: String -> IO ()
compile path = do fs <- readform path
                  writeFile "Cforms.hs"
                    ("module Cforms (cforms) where\n" ++
                     "import KForm\n" ++
                     "cforms :: [((Int,String),[KForm])]\n" ++
                     "cforms = " ++ (show fs) ++ "\n")
                  return ()

indent :: Int -> String
indent d = replicate d ' '

implode :: String -> [String] -> String
implode sep xs = foldl1 (\x y -> x ++ sep ++ y) xs

fmt1 :: Int -> [KForm] -> IO ()
fmt1 _ []     = return ()
fmt1 d (x:xs) = do putStrLn (indent d ++ s)
                   fmt1 d xs
                   return ()
                where
                s =  "(" ++ (show (index x)) ++ ") " ++
                     (implode "," [name x,gobi x,ygobi x,pgobi x])

fmt0 :: Int -> [((Int,String),[KForm])] -> IO ()
fmt0 _ []     = return ()
fmt0 d (x:xs) = do let ((idx,kname),kforms) = x
                   putStrLn ("{" ++ (show idx) ++ "} " ++ kname)
                   fmt1 (d+2) kforms
                   fmt0 d xs
                   return ()

pp :: [((Int,String),[KForm])] -> IO ()
pp xs = fmt0 0 xs

{-
test = do fs <- readform "cforms.cha"
          pp fs
          return ()
-}
