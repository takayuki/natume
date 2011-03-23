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
module Lib (
    castInt,cint,call,
    connect_init,connect_free,connect_start,connect_stop,
    connect_add,connect_build,connect_load,connect_unload,
    connect_search,connect_fetch,connect_dump,
    dic_init,dic_free,dic_start,dic_stop,dic_add,dic_build,
    dic_load,dic_unload,dic_update,dic_insert,
    dic_search,dic_fetch,dic_dump,
    canna_init,canna_free,canna_accept,canna_establish,
    canna_request,canna_response,
  ) where

import CString
import CTypes
import Foreign

castInt :: (Integral a, Integral b) => a -> b
castInt = fromInteger . toInteger

cint :: Int -> CInt
cint = castInt

call :: IO CInt -> IO Int
call x = x >>= (return . castInt)

foreign import ccall "static connect.h connect_init"
  connect_init :: CString -> CString -> IO CInt
foreign import ccall "static connect.h connect_free"
  connect_free :: CInt -> IO ()
foreign import ccall "static connect.h connect_start"
  connect_start :: CInt -> IO CInt
foreign import ccall "static connect.h connect_stop"
  connect_stop :: CInt -> IO ()
foreign import ccall "static connect.h connect_add"
  connect_add :: CInt -> (Ptr CInt) -> CInt -> CInt -> IO CInt
foreign import ccall "static connect.h connect_build"
  connect_build :: CInt -> IO CInt
foreign import ccall "static connect.h connect_load"
  connect_load :: CInt -> IO CInt
foreign import ccall "static connect.h connect_unload"
  connect_unload :: CInt -> IO ()
foreign import ccall "static connect.h connect_search"
  connect_search :: CInt -> (Ptr CInt) -> CInt -> IO CInt
foreign import ccall "static connect.h connect_fetch"
  connect_fetch :: CInt -> CInt -> (Ptr CInt) -> CInt -> (Ptr CInt) -> IO CInt
foreign import ccall "static connect.h connect_dump"
  connect_dump :: CInt -> IO ()

foreign import ccall "static dic.h dic_init"
  dic_init :: CString -> CString -> CString -> CString -> IO CInt
foreign import ccall "static dic.h dic_free"
  dic_free :: CInt -> IO ()
foreign import ccall "static dic.h dic_start"
  dic_start :: CInt -> IO CInt
foreign import ccall "static dic.h dic_stop"
  dic_stop :: CInt -> IO ()
foreign import ccall "static dic.h dic_add"
  dic_add :: CInt -> CString -> CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "static dic.h dic_build"
  dic_build :: CInt -> IO CInt
foreign import ccall "static dic.h dic_load"
  dic_load :: CInt -> IO CInt
foreign import ccall "static dic.h dic_unload"
  dic_unload :: CInt -> IO ()
foreign import ccall "static dic.h dic_update"
  dic_update :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "static dic.h dic_insert"
  dic_insert :: CInt -> CString -> CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "static dic.h dic_search"
  dic_search :: CInt -> CString -> CInt -> IO CInt
foreign import ccall "static dic.h dic_fetch"
  dic_fetch :: CInt -> CInt -> (Ptr CString) -> (Ptr CString) ->
             (Ptr CInt) -> (Ptr CInt) ->
             (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> CInt -> CInt -> IO CInt
foreign import ccall "static dic.h dic_dump"
  dic_dump :: CInt -> IO ()

foreign import ccall "static canna.h canna_init"
  canna_init :: CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "static canna.h canna_free"
  canna_free :: CInt -> IO ()
foreign import ccall "static canna.h canna_accept"
  canna_accept :: CInt -> (Ptr CInt) -> (Ptr CInt) ->
                  (Ptr CInt) -> CString -> CInt -> IO CInt
foreign import ccall "static canna.h canna_establish"
  canna_establish :: CInt -> (Ptr CInt) -> (Ptr CInt) ->
                     (Ptr CInt) -> (Ptr CInt) -> IO CInt
foreign import ccall "static canna.h canna_request"
  canna_request :: CInt -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) ->
    (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> CString -> CInt -> IO CInt
              
foreign import ccall "static canna.h canna_response"
  canna_response :: CInt -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) ->
    (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CString) -> IO CInt

