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
module Hinsi (
    Hinsi(MkHinsi),
    index,path,daughter,name,depth,kt,ktype,
    merge,enum,readclass,
    isprefixof,getbyname,compile,
    pp,lp,test
  ) where

import System.IO
import qualified Katuyou
import qualified KType
import ScmParse hiding (pp)
import ScmToken

data Hinsi = MkHinsi { index    :: Int,
                       path     :: [Int],
                       daughter :: [Hinsi],
                       name     :: [String],
                       depth    :: Int,
                       kt       :: Bool,
                       ktype    :: [KType.KType] }
             deriving (Show)

root :: Hinsi
root = MkHinsi 0 [] [] ["BOS/EOS"] 0 False []

eval2 :: (Int,Bool) -> [String] -> [Expr] -> [Hinsi]
eval2 (_,_)  _ []     = []
eval2 (d,k) ns (x:xs) = (eval1 (d,k) ns x) : (eval2 (d,k) ns xs)

eval1 :: (Int,Bool) -> [String] -> Expr -> Hinsi
eval1 (d,k) ns x =
  case x of
    (ExprList ((ExprName n):(ExprName "%"):ys)) ->
      MkHinsi 0 [] (eval2 ((d+1),True) (ns++[n]) ys) (ns++[n]) (d+1) True []
    (ExprList ((ExprName n):ys)) ->
      MkHinsi 0 [] (eval2 ((d+1),k) (ns++[n]) ys) (ns++[n]) (d+1) k []
    _ ->
      error "Hinsi.eval2: illegal format"

eval :: [Expr] -> [Hinsi]
eval []     = []
eval (x:xs) = (eval1 (0,False) [] x) : eval xs

merge :: [Hinsi] -> [([String],[KType.KType])] -> [Hinsi]
merge []     _  = []
merge (x:xs) ts = (x {daughter=d,ktype=k}) : merge xs ts
                  where
                  d = merge (daughter x) ts
                  k = let ks = filter (((name x)==) . fst) ts in
                        if null ks then [] else snd (head ks)

enum0 :: (Int,[Int]) -> [Hinsi] -> (Int,[Hinsi])
enum0 (n,_) []     = (n,[])
enum0 (n,p) (x:xs) = (n2,[x1] ++ xs1 ++ xs2)
                     where
                     (n1,xs1) = enum0 (n+1,p++[n]) (daughter x)
                     (n2,xs2) = enum0 (n1,p) xs
                     x1 = (x {index=n,path=p++[n],daughter=xs1})

enum :: [Hinsi] -> [Hinsi]
enum xs = snd (enum0 (0,[]) xs)

readclass :: String -> String -> String -> IO [Hinsi]
readclass g f t = do h <- openFile g ReadMode
                     hSetEncoding h latin1
                     s <- hGetContents h
                     let gs = (eval (parse (tokenize s)))
                     ts <- Katuyou.readtypeform f t
                     return (enum (merge (root : gs) ts))

isprefixof :: [String] -> [String] -> Bool
isprefixof []       _      = True
isprefixof ["*"]    []     = True
isprefixof _        []     = False
isprefixof ("*":xs) (_:ys) = isprefixof xs ys
isprefixof (x:xs)   (y:ys) = if x == y
                             then isprefixof xs ys
                             else False

getbyname :: [String] -> [Hinsi] -> [Hinsi]
getbyname _   []     = []
getbyname key (x:xs) = if isprefixof key (name x)
                       then x : getbyname key xs else getbyname key xs

compile :: String -> IO ()
compile file = do h <- openFile file ReadMode
                  hSetEncoding h latin1
                  s <- hGetContents h
                  let gs = (eval (parse (tokenize s)))
                  writeFile "Grammar.hs"
                    ("module Grammar (grammar) where\n" ++
                     "import Hinsi\n" ++
                     "grammar :: [Hinsi]\n" ++
                     "grammar = " ++ (show (root : gs)) ++ "\n")
                  return ()

indent :: Int -> String
indent d = replicate d ' '

fmt0 :: Int -> [Hinsi] -> IO ()
fmt0 _ []     = return ()
fmt0 d (x:xs) = do putStr (indent d)
                   putStrLn ((show (index x)) ++ " " ++
                             (show (path x))  ++ " " ++
                             foldl1 (\y z -> y ++ "-" ++ z) (name x))
                   Katuyou.fmt1 (d+2) (ktype x)
                   fmt0 ((depth x) * 2) (daughter x)
                   fmt0 d xs
                   return ()

pp :: [Hinsi] -> IO ()
pp xs = fmt0 0 xs

lp :: [Hinsi] -> IO ()
lp xs = do mapM (\h ->
                   do putStr ((show (index h)) ++ " " ++
                              (show (path h)) ++ " ")
                      putStrLn (foldl1 (\ x y -> x ++ "-" ++ y) (name h)))
                xs
           return ()

test :: IO ()
test = do hs <- readclass "grammar.cha" "cforms.cha" "ctypes.cha"
          pp hs
          return ()

