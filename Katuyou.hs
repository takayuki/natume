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
module Katuyou (
    typeform,readtypeform,pp,fmt0,fmt1
  ) where

import qualified KForm
import qualified KType

basicform :: [KForm.KForm] -> KForm.KForm
basicform []     = error "Katuyou.basicform: no basic form"
basicform (x:xs) = if (KForm.name x) == "´ðËÜ·Á"
                   then x
                   else basicform xs

getform :: String -> [((Int,String),[KForm.KForm])] ->
           ((Int,String),[KForm.KForm])
getform _   []     = error "Katuyou.getform: no form with given name"
getform key (x:xs) = let ((_,k),_) = x in
                     if key == k then x else getform key xs

typeform :: [([String],[String])] -> [((Int,String),[KForm.KForm])] ->
            [([String],[KType.KType])]
typeform []     _  = []
typeform (x:xs) fs =
  let (hname,knames) = x
      types = map (\((idx,name),forms) ->
                     KType.MkKType idx name (basicform forms) forms)
                  (map (\name -> getform name fs) knames) in
    ((hname,types) : (typeform xs fs))

readtypeform :: String -> String -> IO [([String],[KType.KType])]
readtypeform f t =  do fs <- KForm.readform f
                       ts <- KType.readtype t
                       return (typeform ts fs)

indent :: Int -> String
indent d = replicate d ' '

fmt1 :: Int -> [KType.KType] -> IO ()
fmt1 _ []     = return ()
fmt1 d (x:xs) = do putStrLn (indent d ++ s)
                   KForm.fmt1 (d+4) [KType.basic x]
                   KForm.fmt1 (d+2) (KType.kform x)
                   fmt1 d xs
                   return ()
                where
                s = "{" ++ (show (KType.index x)) ++ "} " ++ (KType.name x)

fmt0 :: Int -> [([String],[KType.KType])] -> IO ()
fmt0 _ []     = return ()
fmt0 d (x:xs) = let (hname,ktypes) = x in
                do KType.fmt1 d hname
                   fmt1 (d+2) ktypes
                   fmt0 d xs
                   return ()

pp :: [([String],[KType.KType])] -> IO ()
pp xs = fmt0 0 xs

{-
test = do ts <- readtypeform "cforms.cha" "ctypes.cha"
          pp ts
          return ()
-}
