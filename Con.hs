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
module Con (
    Con,connect_init,connect_search,connect_free
  ) where

import Prelude hiding (id,last)
import Foreign.C.String
import Foreign.C.Types
import Foreign
import qualified Lib
import Lib (castInt,cint,call)

type Con = (Int,(Ptr CInt),(Ptr CInt))

width :: Int
width = 32

connect_init :: String -> String -> IO Con
connect_init idx dat =
  do cidx <- newCString idx
     cdat <- newCString dat
     id <- call (Lib.connect_init cidx cdat)
     if id == -1
       then error "unable to initialize external library"
       else return ()
     status <- call (Lib.connect_load (cint id))
     if status == -1
       then error ("unable to load connection rule: " ++
                   idx ++ "," ++ dat)
       else return ()
     rule <- mallocArray width
     cost <- malloc
     free cdat
     free cidx
     return (id,rule,cost)

fetch1 :: Con -> Int -> Int -> IO [([Int],Int)]
fetch1 con i n =
  let (id,rule,cost) = con in
    if i < n
    then do num <- call (Lib.connect_fetch (cint id) (cint i) rule
                          (cint width) cost)
            r <- peekArray num rule
            c <- peek cost
            fs <- fetch1 con (i+1) n
            return ((map castInt r,castInt c):fs)
    else return []

connect_search :: Con -> [Int] -> IO [([Int],Int)]
connect_search con key = let (id,_,_) = con in
                           do rule <- newArray (map castInt key)
                              num <- call (Lib.connect_search (cint id) rule
                                       (cint (length key)))
                              ret <- fetch1 con 0 num
                              free rule
                              return ret

connect_free :: Con -> IO ()
connect_free con = let (id,rule,cost) = con in
                     do free rule
                        free cost
                        Lib.connect_unload (cint id)
                        Lib.connect_free (cint id)
                        return ()
