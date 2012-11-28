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

import System.Environment
import Canna
import Getopt
import qualified Config

version :: IO ()
version = putStrLn Config.version

help :: IO ()
help = mapM_ putStrLn ["natume [-Shv] [-d socket]"]

mainDo :: (String,Int) -> IO ()
mainDo (sock,mode) = cannaLoop sock mode

mainOpts :: ([Opt],[String]) -> (String,Int) -> IO ()
mainOpts ([],args) (sock,mode) =
  do if null args
       then mainDo (sock,mode)
       else help
mainOpts ((x:xs),args) (sock,mode) =
  do case x of
      (OptFlg 'S') ->
        mainOpts (xs,args) (sock,1)
      (OptArg 'd' arg) ->
        mainOpts (xs,args) (arg,mode)
      (OptFlg 'v') ->
        version
      _ ->
        help

main :: IO ()
main = do as <- getArgs
          let (opts,args) = getopt as "Sd:vh" in
            case opts of
              [(OptErr s)] ->
                do putStrLn s
                   help
              _            ->
                mainOpts (opts,args) ("",0)
