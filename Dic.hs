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
module Dic (
    Dic,Mrph(MkMrph),DicWord,mkdicword,
    bos,eos,glue,number,strange,
    dic_init,dic_search,dic_update,dic_insert,dic_free
  ) where

import Prelude hiding (id,last)
import Foreign.C.Types
import Foreign.C.String
import Foreign
import System.Environment
import Compile (rensetu_tbl2)
import qualified Config
import qualified Lib
import qualified Rensetu
import Lib (castInt,cint,call)

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

malloc2 :: (Storable a) => Int -> Int -> IO (Ptr (Ptr a))
malloc2 w h = do p <- mallocArray h
                 q <- chunk w h
                 pokeArray p q
                 return p
             where
             chunk _ 0 = return []
             chunk s n = do x <- mallocArray s
                            xs <- chunk s (n-1)
                            return (x:xs)

free2 :: (Storable a) => (Ptr (Ptr a)) -> IO ()
free2 pp = do p <- peekArray height pp
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
     id <- call (Lib.dic_init cidx cdat csta nullPtr)
     if id == -1
       then error idx -- return []
       else
         do status <- call (Lib.dic_load (cint id))
            if status == -1
              then return []
              else do yomi  <- malloc2 width height
                      word  <- malloc2 width height
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
peekCInt = return . castInt

fetch2 :: Dic -> Int -> Int -> IO [Mrph]
fetch2 dic i num =
  let (id,yomi,word,table,cost,point,style,last) = dic in
    if i < num
    then do y <- peekElemOff yomi i >>= peekCAString
            w <- peekElemOff word i >>= peekCAString
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
    then do num' <- call (Lib.dic_fetch (cint id) (cint i) yomi word table cost
                            point style last (cint width) (cint height))
            fs <- fetch2 dic 0 num'
            gs <- fetch1 dic (i+1) num
            return (fs ++ gs)
    else return []

dic_search :: Dic -> String -> Int -> IO [Mrph]
dic_search dic key exact =
  let (id,_,_,_,_,_,_,_) = dic in
    do ckey <- newCAString key
       num <- call (Lib.dic_search (cint id) ckey (cint exact))
       ret <- fetch1 dic 0 num
       free ckey
       return ret

dic_update :: Dic -> Int -> Int -> IO Int
dic_update dic pnt stl = let (id,_,_,_,_,_,_,_) = dic in
                         call (Lib.dic_update (cint id) (cint pnt) (cint stl))

dic_insert :: Dic -> String -> String -> Int -> Int -> Int -> IO Int
dic_insert dic y w tbl cst stl =
  let (id,_,_,_,_,_,_,_) = dic in
    do yomi <- newCAString y
       word <- newCAString w
       status <- call (Lib.dic_insert (cint id) yomi word (cint tbl)
                         (cint cst) (cint stl))
       free word
       free yomi
       return status

dic_free :: Dic -> IO ()
dic_free dic =
  let (id,yomi,word,table,cost,point,style,last) = dic in
    do free2 yomi
       free2 word
       free table
       free cost
       free point
       free style
       free last
       Lib.dic_unload (cint id)
       Lib.dic_free (cint id)
       return ()

