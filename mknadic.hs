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
module Main where

import Prelude hiding (id)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Monad
import System.Environment
import qualified Config
import qualified MkDic
import Getopt
import Lib
import Re

version :: IO ()
version = putStrLn Config.version

help :: IO ()
help = mapM_ putStrLn ["mknadic [-hv] [-I dir] [-d dir] [-f base] [...]"]

readDics :: Int -> Int -> String -> [String] -> IO ()
readDics _  _     _   []     = return ()
readDics id start dir (x:xs) =
  do ds <- MkDic.readdic start (dir ++ Config.pathsep ++ x)
     next <- foldM
               (\prev (MkDic.MkLex _ w y _ _ table cost point) ->
                  if null y
                  then return prev
                  else do yomi <- newCAString (kata2hira y)
                          word <- newCAString w
                          status <- call (dic_add (cint id) yomi word
                                      (cint table) (cint cost) (cint point))
                          free yomi
                          free word
                          if status == -1
                            then error "unable to register word in dictonary"
                            else return (point+1)
               ) start ds
     putStrLn (x ++ " " ++ (show (next - start)))
     readDics id next dir xs

mainDo :: (String,String,[String],[String]) -> IO ()
mainDo (base,dir,dics,_) =
  do idx <- newCString (base ++ ".idx")
     dat <- newCString (base ++ ".dat")
     sta <- newCString (base ++ ".sta")
     tmp <- newCString (base ++ ".tmp")
     id <- call (dic_init idx dat sta tmp)
     if id == -1
       then error "unable to initialize external library"
       else return ()
     status <- dic_start (cint id)
     if status == -1
       then error "unable to initialize dictionary"
       else return ()
     readDics id 1 dir dics
     dic_stop (cint id)
     status' <- call (dic_build (cint id))
     if status' == -1
       then error "unable to build dictionary successfully"
       else return ()
     dic_free (cint id)
     return ()

mainOpts :: ([Opt],[String]) -> (String,String,[String]) -> IO ()
mainOpts ([],args) (base,dir,aux) =
  do if null args
       then mainDo (base,dir,["User.dic"],aux)
       else mainDo (base,dir,args,aux)
mainOpts ((x:xs),args) (base,dir,aux) =
  do case x of
      (OptArg 'I' arg) ->
        mainOpts (xs,args ++ (map (\f -> arg ++ Config.pathsep ++ f)
                                  Config.ipadics))
                 ("ipadic",dir,aux)
      (OptArg 'd' arg) ->
        mainOpts (xs,args) (base,arg,aux)
      (OptArg 'f' arg) ->
        mainOpts (xs,args) (arg,dir,aux)
      (OptFlg 'v') ->
        version
      _ ->
        help

main :: IO ()
main = do as <- getArgs
          let (opts,args) = getopt as "I:X:d:f:vh" in
            case opts of
              [(OptErr s)] ->
                do putStrLn s
                   help
              _            ->
                mainOpts (opts,args) ("user",".",[])
