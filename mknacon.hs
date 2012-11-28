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
module Main where

import Prelude hiding (id)
import Foreign.C.String
import Foreign
import Control.Monad
import System.Environment
import qualified Config
import qualified Connect
import qualified Rensetu
import Compile
import Getopt
import Lib

version :: IO ()
version = putStrLn Config.version

help :: IO ()
help = mapM_ putStrLn ["mknacon [-hv] [-I dir] [-d dir] [-f base]"]

mainDo :: (String,String) -> IO ()
mainDo (base,dir) =
  do idx <- newCString (base ++ ".idx")
     dat <- newCString (base ++ ".dat")
     id <- call (connect_init idx dat)
     if id == -1
       then error "unable to initialize external library"
       else return ()
     status <- connect_start (cint id)
     if status == -1
       then error "unable to initialize dictionary"
       else return ()
     cs <- Connect.readconn (dir ++ Config.pathsep ++ "connect.cha")
     foldM (\n (connect,cost) ->
              do key <- newArray0 0 (map castInt (reverse connect))
                 status' <- call (connect_add (cint id) key
                                  (cint (length connect)) (cint cost))
                 if status' == -1
                   then error "unable to register rule in dictonary"
                   else return (n+1)
           ) (1::Int) (filter ((2<=).length.fst)
                              (Rensetu.convert cs
                               (rensetu_tbl1 ++ rensetu_tbl2)))
     connect_stop (cint id)
     status' <- call (connect_build (cint id))
     if status' == -1
       then error "unable to build dictionary successfully"
       else return ()
     connect_free (cint id)
     return ()

mainOpts :: ([Opt],[String]) -> (String,String) -> IO ()
mainOpts ([],args) (base,dir) =
  do if null args
       then mainDo (base,dir)
       else help
mainOpts ((x:xs),args) (base,dir) =
  do case x of
      (OptArg 'I' arg) ->
        mainOpts (xs,args) ("connect",arg)
      (OptArg 'd' arg) ->
        mainOpts (xs,args) (base,arg)
      (OptArg 'f' arg) ->
        mainOpts (xs,args) (arg,dir)
      (OptFlg 'v') ->
        version
      _ ->
        help

main :: IO ()
main = do as <- getArgs
          let (opts,args) = getopt as "I:d:f:vh" in
            case opts of
              [(OptErr s)] ->
                do putStrLn s
                   help
              _            ->
                mainOpts (opts,args) ("connect",".")

