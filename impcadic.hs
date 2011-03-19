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

import System.IO
import Char
import System
import qualified Config
import Getopt
import Re

version :: IO ()
version = putStrLn Config.version

help :: IO ()
help = mapM_ putStrLn ["impcadic [-hv] [-I dir] [-d dir] [-f base] [...]"]

readline :: Handle -> IO String
readline h = do eof <- hIsEOF h
                if eof then return [] else hGetLine h

readlines :: Handle -> IO [String]
readlines h = do l <- readline h
                 if l == "" then return []
                            else do ls <- readlines h
                                    return (l:ls)

atoi :: String -> Int
atoi s =  foldl (\x y -> x * 10 + y) 0 ((map (\c -> ord c - ord '0')) s)

cost :: Int -> Int -> String
cost b n = if (b-n) < (0::Int) then (show (0::Int)) else (show (b-n))

escape :: String -> String
escape [] = []
escape ('"':xs) = '\\' : '"' : (escape xs)
escape ('\\':xs) = '\\' : '\\' : (escape xs)
escape (x:xs) = x : (escape xs)

ipa :: String -> String -> String -> Int -> Int -> String
ipa yomi word hinsi base count =
  "(ÉÊ»ì " ++ hinsi ++ ") " ++
  "((¸«½Ð¤·¸ì (\"" ++ (escape word) ++ "\" " ++ (cost base count) ++ "))" ++
  " (ÆÉ¤ß \"" ++ (escape (hira2kata yomi)) ++ "\")" ++ 
  " (È¯²» \"" ++ (escape (hira2kata yomi)) ++ "\"))"

parse1 :: String -> (String,Int) -> [String] -> [String]
parse1 _    _            []     = []
parse1 yomi (hinsi,count) (x:xs) =
  case x of
    ('#':t) -> let hinsi' = takeWhile ('*'/=) t
                   count' = let cnt = dropWhile ('*'/=) t in
                              if cnt == [] then 0 else atoi (tail cnt)
               in
                 parse1 yomi (hinsi',count') xs
    word    ->
      (case hinsi of
         -- ¶¦ÄÌÉÊ»ìÊ¬ÎàÉ½ (Canna37p3/dic/ideo/grammar/main.code)
         -- Æ°»ì
         "K5"  -> []
         "K5r" -> []
         "C5r" -> []
         "G5"  -> []
         "G5r" -> []
         "S5"  -> []
         "S5r" -> []
         "T5"  -> []
         "T5r" -> []
         "N5"  -> []
         "B5"  -> []
         "B5r" -> []
         "M5"  -> []
         "M5r" -> []
         "R5"  -> []
         "R5r" -> []
         "L5"  -> []
         "W5"  -> []
         "W5r" -> []
         "U5"  -> []
         "U5r" -> []
         "KS"  -> []
         "KSr" -> []
         "KX"  -> []
         "SX"  -> []
         "ZX"  -> []
         "NZX" -> []
         -- ÂÎ¸À
         "KJ"  -> [ipa yomi word "(¸ìÃÇÊÒ)" 10000 count]
         "CN"  -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ÃÏ°è °ìÈÌ)" 3000 count]
         "CNS" -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ÃÏ°è °ìÈÌ)" 3000 count]
         "JN"  -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ¿ÍÌ¾ °ìÈÌ)" 3000 count]
         "JNS" -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ¿ÍÌ¾ À«)"   3000 count]
         "JNM" -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ¿ÍÌ¾ Ì¾)"   3000 count]
         "JCN" -> [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ¿ÍÌ¾ °ìÈÌ)" 3000 count,
                   ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ÃÏ°è °ìÈÌ)" 3000 count]
         "KK" ->  [ipa yomi word "(Ì¾»ì ¸ÇÍ­Ì¾»ì ÁÈ¿¥)"      3000 count]
         "T00" -> []
         "T01" -> []
         "T02" -> []
         "T03" -> []
         "T04" -> []
         "T05" -> [ipa yomi word "(Ì¾»ì ·ÁÍÆÆ°»ì¸ì´´)" 3000 count]
         "T06" -> []
         "T07" -> []
         "T08" -> []
         "T09" -> []
         "T10" -> []
         "T11" -> []
         "T12" -> []
         "T13" -> []
         "T14" -> []
         "T15" -> []
         "T16" -> []
         "T17" -> []
         "T18" -> []
         "T19" -> []
         "T20" -> []
         "T21" -> []
         "T22" -> []
         "T23" -> []
         "T24" -> []
         "T25" -> []
         "T26" -> []
         "T27" -> []
         "T28" -> []
         "T29" -> []
         "T30" -> [ipa yomi word "(Ì¾»ì ¥µÊÑÀÜÂ³)" 3000 count]
         "T31" -> []
         "T32" -> []
         "T33" -> []
         "T34" -> []
         "T35" -> [ipa yomi word "(Ì¾»ì °ìÈÌ)" 3000 count]
         "T36" -> []
         "T37" -> []
         "T38" -> []
         "T39" -> []
         -- Éû»ì
         "F00" -> []
         "F01" -> []
         "F02" -> []
         "F03" -> []
         "F04" -> []
         "F05" -> []
         "F06" -> []
         "F07" -> []
         "F08" -> []
         "F09" -> []
         "F10" -> []
         "F11" -> []
         "F12" -> []
         "F13" -> []
         "F14" -> []
         "F15" -> []
         -- ·ÁÍÆ»ì
         "KY"  -> []
         "KYT" -> []
         "KYna" -> []
         "KYmi" -> []
         "KYme" -> []
         "KYmime" -> []
         "KYU" -> []
         -- ¤½¤ÎÂ¾
         "CJ" -> [ipa yomi word "(´¶Æ°»ì)" 3000 count]
         "RT" -> []
         "OKX" -> []
         -- ¥ï¡¼¥×¥íÍÑÉ¸½àÆüËÜ¸ìÊ¸Ë¡ (Canna37p3/dic/ideo/grammar/gram.code)
         "JT"   -> []
         "JTNO" -> []
         "JTNN" -> []
         "NN"   -> []
         "N00"  -> []
         "N01"  -> []
         "N02"  -> []
         "N03"  -> []
         "KN"   -> []
         "TKN"  -> []
         "FKN"  -> []
         "PRE"  -> []
         "CNPRE" -> []
         "JNPRE" -> []
         "NNPRE" -> []
         "SNPRE" -> []
         "SUC"   -> []
         "CNSUC1" -> []
         "CNSUC2" -> []
         "JNSUC"  -> []
         "JS"    -> []
         "JSSUC" -> []
         "N2T10" -> []
         "N2T15" -> []
         "N2T16" -> []
         "N2T30" -> []
         "N2T35" -> []
         "D2T16" -> []
         "D2T30" -> []
         "D2T35" -> []
         "ND2KY" -> []
         "N2KY"  -> []
         "D2KY"  -> []
         ('K':t:_) -> if isLower t
                      then [ipa yomi word "(½õ»ì ³Ê½õ»ì °ìÈÌ)" 3000 count]
                      else error ("unknown type: " ++ hinsi)
         ('F':t:_) -> if isLower t
                      then [ipa yomi word "(Éû»ì °ìÈÌ)" 3000 count]
                      else error ("unknown type: " ++ hinsi)
         ('S':t:_) -> if isLower t
                      then [ipa yomi word "(½õ»ì ÀÜÂ³½õ»ì)" 3000 count]
                      else error ("unknown type: " ++ hinsi)
         ('Z':t:_) -> if isLower t
                      then [ipa yomi word "(½õ»ì ½ª½õ»ì)" 3000 count]
                      else error ("unknown type: " ++ hinsi)
         (t:_)     -> if isLower t
                      then []
                      else error ("unknown type: " ++ hinsi)
         _         -> error ("unknown type: " ++ hinsi)
      ) ++ (parse1 yomi (hinsi,count) xs)

parse0 :: String -> [String] -> [String]
parse0 _    []   = []
parse0 yomi (x:xs) =
  case x of
    ('#':t) -> let hinsi = takeWhile ('*'/=) t
                   count = let cnt = dropWhile ('*'/=) t in
                             if cnt == [] then 0 else atoi (tail cnt)
               in
                 parse1 yomi (hinsi,count) xs
    _       -> error "parse0"

parse :: String -> [String]
parse entry = let ws = words entry in
                if ws == [] then []
                            else let (yomi:rest) = ws in
                                   parse0 yomi rest

convert :: Handle -> String -> [String] -> IO ()
convert _ _   []     = return ()
convert o dir (x:xs) =
  do i <- openFile (dir ++ Config.pathsep ++ x) ReadMode
     hSetEncoding i latin1
     ls <- readlines i
     mapM_ (\l -> mapM_ (hPutStrLn o) (parse l)) ls
     hFlush o
     convert o dir xs

mainDo :: (String,String,[String]) -> IO ()
mainDo (base,dir,dics) =
  do o <- openFile (base ++ ".dic") WriteMode
     hSetEncoding o latin1
     convert o dir dics

mainOpts :: ([Opt],[String]) -> (String,String) -> IO ()
mainOpts ([],args) (base,dir) =
  do if null args
       then mainDo (base,dir,Config.cannadics)
       else mainDo (base,dir,args)
mainOpts ((x:xs),args) (base,dir) =
  do case x of
      (OptArg 'I' arg) ->
        mainOpts (xs,args ++ Config.cannadics) ("Cannadic",arg)
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
                mainOpts (opts,args) ("Cannadic",".")

