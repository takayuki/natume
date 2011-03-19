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
module Dic (
    Dic,Mrph(MkMrph),DicWord,mkdicword,
    bos,eos,glue,number,strange,
    dic_init,dic_search,dic_update,dic_insert,dic_free
  ) where

import Prelude hiding (id,last)
import CTypes
import CString
import Foreign
import System
import Compile (rensetu_tbl2)
import qualified Config
import qualified Lib
import qualified Rensetu

type Dic = (Int,(Ptr CString),(Ptr CString),
            (Ptr CInt),(Ptr CInt),(Ptr CInt),(Ptr CInt),(Ptr CInt))

data Mrph = MkMrph String String Int Int Int Int Int Int
            deriving (Show)

type DicWord = (String,Int,Int,Int)

mkdicword :: Mrph -> DicWord
mkdicword (MkMrph _ w _ _ d p s _) =(w,d,p,s)

bos,eos :: Mrph
bos = MkMrph "<開始>" "<開始>" 0 0 0 0 0 0
eos = MkMrph "<終了>" "<終了>" 0 0 0 0 0 0

rensetu :: [String] -> Int
rensetu name = Rensetu.index
                 (head (filter (Rensetu.match (name,"","","")) rensetu_tbl2))

glue,number,strange :: Int
glue = 0
number = rensetu ["名詞","数"]
strange = rensetu ["名詞","サ変接続"]

width,height :: Int
(width,height) = (64,1024)

alloc2 :: (Storable a) => Int -> Int -> IO (Ptr (Ptr a))
alloc2 w h = do p <- mallocArray h
                q <- chunk w h
                pokeArray p q
                return p
             where
             chunk _ 0 = return []
             chunk s n = do x <- mallocArray s
                            xs <- chunk s (n-1)
                            return (x:xs)

unalloc2 :: (Storable a) => (Ptr (Ptr a)) -> IO ()
unalloc2 pp = do p <- peekArray height pp
                 mapM_ free p
                 free pp

newpath :: String -> IO CString
newpath x = if (head x) == '/'
            then newCString x
            else do home <- getEnv "HOME"
                    let natume = home ++ Config.pathsep ++ Config.natumedir
                    newCString (natume ++ Config.pathsep ++ x)

dic_init :: String -> String -> String -> IO [Dic]
dic_init idx dat sta =
  do cidx <- newpath idx
     cdat <- newpath dat
     csta <- newpath sta
     id <- Lib.dic_init cidx cdat csta nullPtr
     if id == -1
       then error idx -- return []
       else
         do status <- Lib.dic_load id
            if status == -1
              then return []
              else do yomi  <- alloc2 width height
                      word  <- alloc2 width height
                      table <- mallocArray height
                      cost  <- mallocArray height
                      point <- mallocArray height
                      style <- mallocArray height
                      last  <- mallocArray height
                      free cdat
                      free cidx
                      free csta
                      return [(id,yomi,word,table,cost,point,style,last)]

peekCInt :: CInt -> IO Int
peekCInt = return . fromInteger . toInteger

fetch2 :: Dic -> Int -> Int -> IO [Mrph]
fetch2 dic i num =
  let (id,yomi,word,table,cost,point,style,last) = dic in
    if i < num
    then do y <- peekElemOff yomi i >>= peekCString
            w <- peekElemOff word i >>= peekCString
            t <- peekElemOff table i >>= peekCInt
            c <- peekElemOff cost i >>= peekCInt
            p <- peekElemOff point i >>= peekCInt
            s <- peekElemOff style i >>= peekCInt
            m <- peekElemOff last i >>= peekCInt
            let f = MkMrph y w t c id p s m
            fs <- fetch2 dic (i+1) num
            return (f:fs)
    else return []

fetch1 :: Dic -> Int -> Int -> IO [Mrph]
fetch1 dic i num =
  let (id,yomi,word,table,cost,point,style,last) = dic in
    if i < num
    then do num' <- Lib.dic_fetch id i yomi word table cost
                                  point style last width height
            fs <- fetch2 dic 0 num'
            gs <- fetch1 dic (i+1) num
            return (fs ++ gs)
    else return []

dic_search :: Dic -> String -> Int -> IO [Mrph]
dic_search dic key exact =
  let (id,_,_,_,_,_,_,_) = dic in
    do ckey <- newCString key
       num <- Lib.dic_search id ckey exact
       ret <- fetch1 dic 0 num
       free ckey
       return ret

dic_update :: Dic -> Int -> Int -> IO Int
dic_update dic pnt stl = let (id,_,_,_,_,_,_,_) = dic in
                           Lib.dic_update id pnt stl

dic_insert :: Dic -> String -> String -> Int -> Int -> Int -> IO Int
dic_insert dic y w tbl cst stl =
  let (id,_,_,_,_,_,_,_) = dic in
    do yomi <- newCString y
       word <- newCString w
       status <- Lib.dic_insert id yomi word tbl cst stl
       free word
       free yomi
       return status

dic_free :: Dic -> IO ()
dic_free dic =
  let (id,yomi,word,table,cost,point,style,last) = dic in
    do unalloc2 yomi
       unalloc2 word
       free table
       free cost
       free point
       free style
       free last
       Lib.dic_unload id
       Lib.dic_free id
       return ()

