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
module Canna (
    cannaLoop,
  ) where

import Prelude hiding (id,init,min)
import CTypes
import CString
import Foreign
import Monad
import System
import Dic
import Re
import Session
import qualified Lib
import Lib (castInt,cint,call)

type Server = (Int,(Ptr CInt),(Ptr CInt),(Ptr CInt),
               (Ptr CInt),(Ptr CInt),(Ptr CInt),CString,(Ptr CString))

bufsize,buf2width,buf2height :: Int
(bufsize,buf2width,buf2height) = (2048,64,256)

yomi :: Mrph -> String
yomi (MkMrph y _ _ _ _ _ _ _) = y

fst4 :: (a,b,c,d) -> a
fst4 (x,_,_,_) = x

malloc2 :: (Storable a) => Int -> Int -> IO (Ptr (Ptr a))
malloc2 w h = do p <- mallocArray h
                 q <- chunk w h
                 pokeArray p q
                 return p
              where
              chunk s n = if 1 <= n then do x <- mallocArray s
                                            xs <- chunk s (n-1)
                                            return (x:xs)
                                    else return []

poke2 :: (Ptr CString) -> [String] -> IO Int
poke2 buf2 = foldM (\n x -> do newCString x >>= pokeElemOff buf2 n
                               return (n+1))
                   (0 :: Int)

peekCInt :: (Ptr CInt) -> IO Int
peekCInt p = peek p >>= (return . castInt)

pokeCInt :: (Ptr CInt) -> Int -> IO ()
pokeCInt x = poke x . castInt

free2 :: (Storable a) => (Ptr (Ptr a)) -> IO ()
free2 p = do q <- peekArray buf2height p
             mapM_ free q
             free p

canna_init :: String -> Int -> IO Server
canna_init socket mode =
  do cpath <- newCString socket
     id <- call (Lib.canna_init cpath (cint 0) (cint 1) (cint mode))
     if id == -1
       then error "unable to initialize external library"
       else return ()
     maj  <- malloc
     min  <- malloc
     cxt  <- malloc
     aux0 <- malloc
     aux1 <- malloc
     aux2 <- malloc
     buf  <- mallocArray bufsize
     buf2 <- malloc2 buf2width buf2height
     free cpath
     return (id,maj,min,cxt,aux0,aux1,aux2,buf,buf2)

canna_free :: Server -> IO ()
canna_free srv =
  let (id,maj,min,cxt,aux0,aux1,aux2,buf,buf2) = srv in
    do free maj
       free min
       free cxt
       free aux0
       free aux1
       free aux2
       free buf
       free2 buf2
       Lib.canna_free (cint id)
       return ()

accept :: Server -> IO Int
accept srv =
  let (id,maj,min,_,aux0,_,_,buf,_) = srv in
    do status <- call (Lib.canna_accept (cint id) maj min aux0 buf
                         (cint bufsize))
       return status

establish :: Server -> Int -> IO Int
establish srv ack =
  let (id,maj,min,cxt,aux0,_,_,_,_) = srv in
    do pokeCInt cxt ack
       poke aux0 3
       status <- call (Lib.canna_establish (cint id) maj min cxt aux0)
       return status

request :: Server -> IO Int
request srv = 
  let (id,maj,min,cxt,aux0,aux1,aux2,buf,_) = srv in
    do status <- call (Lib.canna_request (cint id) maj min cxt
                         aux0 aux1 aux2 buf (cint bufsize))
       if status == -1
         then do canna_free srv
                 exitWith (ExitFailure 1)
         else return ()
       return status

response :: Server -> IO Int
response srv = 
  let (id,maj,min,cxt,aux0,aux1,aux2,_,buf2) = srv in
    do status <- call (Lib.canna_response (cint id) maj min cxt
                         aux0 aux1 aux2 buf2)
       if status == -1
         then do canna_free srv
                 exitWith (ExitFailure 1)
         else return ()
       return status

type Service = (Server,Session)

-- Finalization
service :: Service -> CInt -> CInt -> IO Service
service s 0x2 0 =
  let (srv,ssn) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         canna_free srv
         session_free ssn
         exitWith ExitSuccess

-- CreateContext
service s 0x3 0 =
  let (srv,(con,dics,cxts)) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do pokeCInt aux0 (length cxts)
         response srv
         dispatch (srv,(con,dics,cxts++[(0,[])]))

-- CreateDictionary
service s 0x3 1 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- DuplicateContext
service s 0x4 0 =
  let (srv,(con,dics,cxts)) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do src <- peekCInt aux0
         if (length cxts) <= src
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do pokeCInt aux0 (length cxts)
                   response srv
                   dispatch (srv,(con,dics,cxts ++ [cxts !! src]))

-- CloseContext
service s 0x5 0 =
  let (srv,(con,dics,cxts)) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do cont <- peekCInt aux0
         poke aux0 0
         response srv
         dispatch (srv,(con,dics,updateContext cont (0,[]) cxts))

-- GetDictionaryList
service s 0x6 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,aux1,_,_,buf2) = srv in
      do let dict = ["bushu","chimei","fuzokugo","hojomwd","hojoswd",
                     "iroha","kanasample","katakana","keishiki",
                     "necgaiji","number","software","suffix","user"]
         pokeCInt aux0 (length dict)
         pokeCInt aux1 ((length dict) + 1)
         poke2 buf2 (dict ++ [""])
         response srv
         dispatch s

-- GetDirectoryList
service s 0x7 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,aux1,_,_,buf2) = srv in
      do poke aux0 0
         poke aux1 1
         poke2 buf2 [""]
         response srv
         dispatch s

-- MountDictionary
service s 0x8 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- Sync
service s 0x8 1 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- UnmountDictionary
service s 0x9 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- GetMountDictionaryList
service s 0xb 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,aux1,_,_,buf2) = srv in
      do poke aux0 0
         poke aux1 1
         poke2 buf2 [""]
         response srv
         dispatch s

-- DefineWord
service s 0xd 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- DeleteWord
service s 0xe 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- BeginConvert
service s 0xf 0 =
  let (srv,ssn@(_,_,cxts)) = s in
    let (_,_,_,cxt,aux0,aux1,_,buf,buf2) = srv in
      do cont <- peekCInt cxt
         mode <- peekCInt aux0
         input <- peekCString buf
         if (length cxts) <= cont
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do ssn' <- beginConvert ssn cont input mode
                   let (_,_,cxts') = ssn'
                   let (_,pause) = cxts' !! cont
                   let conv = map (concatMap (fst4.head.snd)) pause
                   pokeCInt aux0 (length conv)
                   pokeCInt aux1 ((length conv) + 1)
                   poke2 buf2 (conv ++ [""])
                   --mapM_ putStrLn conv
                   response srv
                   dispatch (srv,ssn')

-- EndConvert
service s 0x10 0 =
  let (srv,(_,dics,cxts)) = s in
    let (_,_,_,cxt,aux0,aux1,_,buf,_) = srv in
      do cont <- peekCInt cxt
         idx <- peekCInt aux0
         mode <- peekCInt aux1
         final <- peekArray idx ((castPtr buf) :: Ptr CInt)
         let (_,pause) = cxts !! cont
         if (length cxts) <= cont
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else
             do if mode == 0
                  then return ()
                  else
                    mapM_
                    (\(fin,(MkMrph y _ tbl _ _ _ _ _,cnd)) -> 
                      if (length cnd) <= fin
                      then return () 
                      else do let (_,dic,pnt,stl) = (cnd !! fin)
                              if 1 < (length dics) && tbl == glue &&
                                 stl == 2 && (yomiNotBad y)
                                then dic_insert (last dics) y y strange 0 stl
                                else return 0
                              if dic < (length dics) && 0 < pnt
                                then dic_update (dics !! dic) pnt stl
                                else return 0
                              return ())
                    (zip (map castInt final) (map head pause))
                poke aux0 0
                response srv
                dispatch s

-- GetCandidacyList
service s 0x11 0 =
  let (srv,ssn@(_,_,cxts)) = s in
    let (_,_,_,cxt,aux0,aux1,_,_,buf2) = srv in
      do cont <- peekCInt cxt
         idx <- peekCInt aux0
         let (_,pause) = cxts !! cont
         if (length cxts) <= cont || (length pause) <= idx
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do ssn' <- getCandidates ssn cont idx
                   let (_,_,cxts') = ssn'
                   let (_,pause') = cxts' !! cont
                   let target = pause' !! idx
                   let cand = map fst4 (snd (head target))
                   let intact = concatMap (fst4.head.snd) (tail target)
                   let cand' = map (++intact) cand
                   pokeCInt aux0 (length cand')
                   pokeCInt aux1 ((length cand') + 1)
                   poke2 buf2 (cand' ++ [""])
                   response srv
                   dispatch (srv,ssn')

-- GetYomi
service s 0x12 0 =
  let (srv,(_,_,cxts)) = s in
    let (_,_,_,cxt,aux0,aux1,_,_,buf2) = srv in
      do cont <- peekCInt cxt
         idx <- peekCInt aux0
         let (_,pause) = cxts !! cont
         if (length cxts) <= cont || (length pause) <= idx
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do let target = pause !! idx
                   let y = yomi (fst (head target))
                   let intact = concatMap yomi (map fst (tail target))
                   let y' = y ++ intact
                   pokeCInt aux0 (length (split y'))
                   poke aux1 2
                   poke2 buf2 ([y',""])
                   response srv
                   dispatch s

-- ResizePause
service s 0x1a 0 =
  let (srv,ssn@(_,_,cxts)) = s in
    let (_,_,_,cxt,aux0,aux1,_,_,buf2) = srv in
      do cont <- peekCInt cxt
         idx <- peekCInt aux0
         size <- peekCInt aux1
         let (_,pause) = cxts !! cont
         if (length cxts) <= cont || (length pause) <= idx
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do let target = pause !! idx
                   let y = yomi (fst (head target))
                   let intact = concatMap yomi (map fst (tail target))
                   let y' = y ++ intact
                   let size' = case size of
                                 (-1) -> (length (split y') + 1)
                                 (-2) -> (length (split y') - 1)
                                 _    -> size
                   ssn' <- resizePause ssn cont idx size'
                   let (_,_,cxts') = ssn'
                   let (_,pause') = cxts' !! cont
                   let conv = map (concatMap (fst4.head.snd)) pause'
                   pokeCInt aux0 (length conv)
                   let conv' = drop idx conv
                   --mapM_ putStrLn conv'
                   pokeCInt aux1 ((length conv') + 1)
                   poke2 buf2 (conv' ++ [""])
                   response srv
                   dispatch (srv,ssn')

-- GetHinsi
service s 0x1b 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,aux1,_,_,buf2) = srv in
      do poke aux0 0
         poke aux1 1
         poke2 buf2 ([""])
         response srv
         dispatch s

-- GetLex
service s 0x1c 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- GetStatus
service s 0x1d 0 =
  let (srv,ssn@(_,_,cxts)) = s in
    let (_,_,_,cxt,aux0,_,aux2,_,_) = srv in
      do cont <- peekCInt cxt
         idx <- peekCInt aux0
         let (_,pause) = cxts !! cont
         if (length cxts) <= cont || (length pause) <= idx
           then do poke aux0 (-1)
                   response srv
                   dispatch s
           else do ssn' <- getCandidates ssn cont idx
                   let (_,_,cxts') = ssn'
                   let (_,pause') = cxts' !! cont
                   let target = pause' !! idx
                   let cand = map fst4 (snd (head target))
                   let intact = concatMap (fst4.head.snd) (tail target)
                   let cand' = map (++intact) cand
                   pokeCInt aux2 (length cand')
                   response srv
                   dispatch (srv,ssn')

-- SetApplicationName
service s 0x21 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

-- NoticeGroupName
service s 0x22 0 =
  let (srv,_) = s in
    let (_,_,_,_,aux0,_,_,_,_) = srv in
      do poke aux0 0
         response srv
         dispatch s

service _ major minor =
  error ("Canna.serevice: protocol is not implemented yet " ++
         "(" ++ (show major) ++ "," ++ (show minor) ++ ")")

dispatch :: Service -> IO Service
dispatch s =
  let (srv,_) = s in
    do request srv
       let (_,maj,min,_,_,_,_,_,_) = srv
       major <- peek maj
       minor <- peek min
       service s major minor

cannaLoop :: String -> Int -> IO ()
cannaLoop path mode =
  do ssn <- session_init
     srv <- canna_init path mode
     let (_,_,_,_,_,_,_,buf,_) = srv
     status <- accept srv
     if status == -1
       then error "Canna.canna_loop: unable to accept connection"
       else return ()
     dat <- peekCString buf
     if take 2 dat == "3."
       then do status' <- establish srv 0
               if status' == -1
                 then error "Canna.canna_loop: unable to establish connection"
                 else return ()
               dispatch (srv,ssn)
               return ()
       else do establish srv (-1)
               canna_free srv
               session_free ssn
               return ()
