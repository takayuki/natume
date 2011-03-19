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
    connect_init,connect_free,connect_start,connect_stop,
    connect_add,connect_build,connect_load,connect_unload,
    connect_search,connect_fetch,connect_dump,
    dic_init,dic_free,dic_start,dic_stop,dic_add,dic_build,
    dic_load,dic_unload,dic_update,dic_insert,
    dic_search,dic_fetch,dic_dump,
    canna_init,canna_free,canna_accept,canna_establish,
    canna_request,canna_response
  ) where

import CString
import CTypes
import Foreign

foreign import ccall "static connect.h connect_init"
  connect_init :: CString -> CString -> IO Int
foreign import ccall "static connect.h connect_free"
  connect_free :: Int -> IO ()
foreign import ccall "static connect.h connect_start"
  connect_start :: Int -> IO Int
foreign import ccall "static connect.h connect_stop"
  connect_stop :: Int -> IO ()
foreign import ccall "static connect.h connect_add"
  connect_add :: Int -> (Ptr CInt) -> Int -> Int -> IO Int
foreign import ccall "static connect.h connect_build"
  connect_build :: Int -> IO Int
foreign import ccall "static connect.h connect_load"
  connect_load :: Int -> IO Int
foreign import ccall "static connect.h connect_unload"
  connect_unload :: Int -> IO ()
foreign import ccall "static connect.h connect_search"
  connect_search :: Int -> (Ptr CInt) -> Int -> IO Int
foreign import ccall "static connect.h connect_fetch"
  connect_fetch :: Int -> Int -> (Ptr CInt) -> Int -> (Ptr CInt) -> IO Int
foreign import ccall "static connect.h connect_dump"
  connect_dump :: Int -> IO ()

foreign import ccall "static dic.h dic_init"
  dic_init :: CString -> CString -> CString -> CString -> IO Int
foreign import ccall "static dic.h dic_free"
  dic_free :: Int -> IO ()
foreign import ccall "static dic.h dic_start"
  dic_start :: Int -> IO Int
foreign import ccall "static dic.h dic_stop"
  dic_stop :: Int -> IO ()
foreign import ccall "static dic.h dic_add"
  dic_add :: Int -> CString -> CString -> Int -> Int -> Int -> IO Int
foreign import ccall "static dic.h dic_build"
  dic_build :: Int -> IO Int
foreign import ccall "static dic.h dic_load"
  dic_load :: Int -> IO Int
foreign import ccall "static dic.h dic_unload"
  dic_unload :: Int -> IO ()
foreign import ccall "static dic.h dic_update"
  dic_update :: Int -> Int -> Int -> IO Int
foreign import ccall "static dic.h dic_insert"
  dic_insert :: Int -> CString -> CString -> Int -> Int -> Int -> IO Int
foreign import ccall "static dic.h dic_search"
  dic_search :: Int -> CString -> Int -> IO Int
foreign import ccall "static dic.h dic_fetch"
  dic_fetch :: Int -> Int -> (Ptr CString) -> (Ptr CString) ->
               (Ptr CInt) -> (Ptr CInt) ->
               (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> Int -> Int -> IO Int
foreign import ccall "static dic.h dic_dump"
  dic_dump :: Int -> IO ()

foreign import ccall "static canna.h canna_init"
  canna_init :: CString -> Int -> Int -> Int -> IO Int
foreign import ccall "static canna.h canna_free"
  canna_free :: Int -> IO ()
foreign import ccall "static canna.h canna_accept"
  canna_accept :: Int -> (Ptr CInt) -> (Ptr CInt) ->
                  (Ptr CInt) -> CString -> Int -> IO Int
foreign import ccall "static canna.h canna_establish"
  canna_establish :: Int -> (Ptr CInt) -> (Ptr CInt) ->
                     (Ptr CInt) -> (Ptr CInt) -> IO Int
foreign import ccall "static canna.h canna_request"
  canna_request :: Int -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) ->
              (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> CString -> Int -> IO Int
foreign import ccall "static canna.h canna_response"
  canna_response :: Int -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) ->
                (Ptr CInt) -> (Ptr CInt) -> (Ptr CInt) -> (Ptr CString) -> IO Int

