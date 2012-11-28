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
module KType (
    KType(MkKType),
    index,name,basic,kform,
    readtype,compile,
    pp,fmt0,fmt1
  ) where

import System.IO
import qualified KForm
import ScmParse hiding (pp)
import ScmToken

data KType = MkKType { index  :: Int,
                       name   :: String,
                       basic  :: KForm.KForm,
                       kform  :: [KForm.KForm] }
             deriving (Show)

eval2 :: [Expr] -> [String]
eval2 []     = []
eval2 (x:xs) = f : eval2 xs where ExprName f = x

eval1 :: Expr -> ([String],[String])
eval1 x = (eval2 hname,eval2 kname)
          where
          ExprList [ExprList hname,ExprList kname] = x

eval :: [Expr] -> [([String],[String])]
eval []     = []
eval (x:xs) = (eval1 x) : eval xs

readtype :: String -> IO [([String],[String])]
readtype path = do h <- openFile path ReadMode
                   hSetEncoding h latin1
                   s <- hGetContents h
                   let ks = (eval (parse (tokenize s)))
                   return ks

compile :: String -> IO ()
compile path = do ts <- readtype path
                  writeFile "Ctypes.hs"
                    ("module Ctypes (ctypes) where\n" ++
                     "ctypes :: [([String],[String])]\n" ++
                     "ctypes = " ++ (show ts) ++ "\n")
                  return ()

indent :: Int -> String
indent d = replicate d ' '

fmt1 :: Int -> [String] -> IO ()
fmt1 _ []     = return ()
fmt1 d (x:xs) = do putStrLn (indent d ++ x)
                   fmt1 d xs
                   return ()

fmt0 :: Int -> [([String],[String])] -> IO ()
fmt0 _ []     = return ()
fmt0 d (x:xs) = do let (hname,kname) = x
                   fmt1 0 hname
                   fmt1 (d+2) kname
                   fmt0 d xs
                   return ()

pp :: [([String],[String])] -> IO ()
pp xs = fmt0 0 xs

{-
test = do fs <- readtype "ctypes.cha"
          pp fs
          return ()
-}
