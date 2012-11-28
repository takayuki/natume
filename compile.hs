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
import qualified Config
import qualified Hinsi
import qualified KForm
import qualified KType
import qualified Rensetu
import Getopt

version :: IO ()
version = putStrLn Config.version

help :: IO ()
help = mapM_ putStrLn ["compile [-hv] [-I dir]"]

ipadic :: String -> String -> String
ipadic dir f = dir ++ Config.pathsep ++ f

cforms :: String
cforms  = "cforms.cha"
ctypes :: String
ctypes  = "ctypes.cha"
grammar :: String
grammar = "grammar.cha"
connect :: String
connect = "connect.cha"

mainDo :: String -> IO ()
mainDo dir = do KForm.compile (ipadic dir cforms)
                KType.compile (ipadic dir ctypes)
                Hinsi.compile (ipadic dir grammar)
                Rensetu.compile (ipadic dir grammar) (ipadic dir cforms)
                                (ipadic dir ctypes) (ipadic dir connect)

mainOpts :: ([Opt],[String]) -> String -> IO ()
mainOpts ([],args) dir =
  do if null args
       then mainDo dir
       else help
mainOpts ((x:xs),args) _ =
  do case x of
      (OptArg 'I' arg) ->
        mainOpts (xs,args) arg
      (OptFlg 'v') ->
        version
      _ ->
        help

main :: IO ()
main = do as <- getArgs
          let (opts,args) = getopt as "I:vh" in
            case opts of
              [(OptErr s)] ->
                do putStrLn s
                   help
              _            ->
                mainOpts (opts,args) "."
