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
module Getopt (
    Opt(OptFlg,OptArg,OptErr),getopt
  ) where

data Opt = OptFlg Char
         | OptArg Char String
         | OptErr String
           deriving (Show)

isFlg :: Char -> String -> Bool
isFlg _ []       = False
isFlg c [x]      = x == c
isFlg c (x:y:ys) = if x == c && y /= ':'
                      then True
                      else isFlg c (y:ys)

hasArg :: Char -> String -> Bool
hasArg _ []       = False
hasArg _ [_]      = False
hasArg c (x:y:ys) = if x == c && y == ':'
                    then True
                    else hasArg c (y:ys)

getopt1 :: ([Opt],[String]) -> [String] -> String -> ([Opt],[String])
getopt1 (a1,a2) []     _    = (reverse a1,reverse a2)
getopt1 (a1,a2) (x:xs) opts =
   case x of
     "--"       -> (reverse a1,(reverse a2)++xs)
     ('-':c:cs) ->
       if isFlg c opts
       then if null cs
            then getopt1 ((OptFlg c):a1,a2) xs opts
            else getopt1 ((OptFlg c):a1,a2) (('-':cs):xs) opts
       else if hasArg c opts
            then if null cs
                 then if null xs
                      then ([OptErr e1],[])
                      else let (y:ys) = xs in
                           case y of
                             ('-':_) ->
                               ([OptErr e1],[])
                             _       ->
                               getopt1 ((OptArg c y):a1,a2) ys opts
                 else getopt1 ((OptArg c cs):a1,a2) xs opts
            else ([OptErr e2],[])
       where
       e1 = "argument is required for this option -- " ++ [c]
       e2 = "unknown option -- " ++ [c]
     _          -> 
       getopt1 (a1,x:a2) xs opts

getopt :: [String] -> String -> ([Opt],[String])
getopt args opts = getopt1 ([],[]) args opts

