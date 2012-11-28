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
module Re (
    regex,
    prefixAscii,prefixMultibyte,
    prefixDigit,prefixAlpha,
    prefixFullDigit,prefixFullAlpha,
    prefixHiragana,prefixKatakana,yomiNotBad,
    split,mblen,kata2hira,hira2kata,han2zen,zen2han
  ) where

import Data.Char
import Data.List

type Match a = ([[a]],[a]) -> [([[a]],[a])]

grpadd1 :: Int -> [[a]] -> a -> [[a]]
grpadd1 _ [] _ = []
grpadd1 n (g:gs) x
  | n == 0    = (g ++ [x]) : gs
  | otherwise = g : (grpadd1 (n - 1) gs x)

grpadd :: [Int] -> [[a]] -> a -> [[a]]
grpadd [] gs _ = gs
grpadd (n:ns) gs x = grpadd ns (grpadd1 n gs x) x

cat :: Match a -> Match a -> Match a
cat f g (gs,t) = [(y2,r2) | (y1,r1) <- f (gs,t), (y2,r2) <- g (y1,r1)]
                                    
uni :: Match a -> Match a -> Match a
uni f g (gs,t) = f (gs,t) ++ g (gs,t)

success :: Match a
success (gs,t) = [(gs,t)]

--failure :: Match a
--failure (_,_) = []

closure :: Match a -> Match a
closure f = uni (positive f) success 

positive :: Match a -> Match a
positive f = cat f (closure f)

orwith :: [(a -> Bool)] -> a -> Bool
orwith [] _     = False
orwith (f:fs) c = f c || orwith fs c

isBar,isAst,isPls,isDot,isLPar,isRPar,isLBrk :: Char -> Bool
isBar  = (=='|')
isAst  = (=='*')
isPls  = (=='+')
isDot  = (=='.')
isLPar = (=='(')
isRPar = (==')')
isLBrk = (=='[')

isRsv :: Char -> Bool
isRsv c = orwith [isBar,isAst,isPls,isDot,isLPar,isRPar] c

isLit :: Char -> Bool
isLit c = not (isRsv c)

type Parser a = (Int,[Int],[a]) -> (Int,Match a,[a])

syntaxerror :: a
syntaxerror = error "syntax error in regular expression"

parse :: String -> (Int,Match Char)
parse xs = if r == []
           then (n,m)
           else syntaxerror
           where
           (n,m,r) = expr (0,[0],xs)

expr :: Parser Char
expr (n,ns,xs)
  | xs == []         = (n2,m2,rs2)
  | isRPar (head xs) = (n2,m2,rs2)
  | isBar  (head xs) = (n2,m2,rs2)
  | isLit  (head xs) = (n2,m2,rs2)
  | isDot  (head xs) = (n2,m2,rs2)
  | isLPar (head xs) = (n2,m2,rs2)
  | isLBrk (head xs) = (n2,m2,rs2)
  | otherwise = syntaxerror
    where
    (n1,m1,rs1) = term (n,ns,xs)
    (n2,m2,rs2) = expr' m1 (n1,ns,rs1)

expr' :: Match Char -> Parser Char
expr' t (n,ns,xs)
  | xs == []         = (n,t,[])
  | isRPar (head xs) = (n,t,xs)
  | isBar  (head xs) = (n2,m2,rs2)
  | otherwise = syntaxerror
    where
    (n1,m1,rs1) = term (n,ns,tail xs)
    (n2,m2,rs2) = expr' (uni t m1) (n1,ns,rs1)

term :: Parser Char
term (n,ns,xs)
  | xs == []         = (n,success,[])
  | isRPar (head xs) = (n,success,xs)
  | isBar  (head xs) = (n,success,xs)
  | isLit  (head xs) = (n2,m2,rs2)
  | isDot  (head xs) = (n2,m2,rs2)
  | isLPar (head xs) = (n2,m2,rs2)
  | isLBrk (head xs) = (n2,m2,rs2)
  | otherwise = syntaxerror
    where 
    (n1,m1,rs1) = factor (n,ns,xs)
    (n2,m2,rs2) = term' m1 (n1,ns,rs1)

term' :: Match Char -> Parser Char
term' t (n,ns,xs)
  | xs == []         = (n,t,[])
  | isBar  (head xs) = (n,t,xs)
  | isRPar (head xs) = (n,t,xs)
  | isLit  (head xs) = (n2,m2,rs2)
  | isDot  (head xs) = (n2,m2,rs2)
  | isLPar (head xs) = (n2,m2,rs2)
  | isLBrk (head xs) = (n2,m2,rs2)
  | otherwise = syntaxerror
      where 
      (n1,m1,rs1) = factor (n,ns,xs)
      (n2,m2,rs2) = term' (cat t m1) (n1,ns,rs1)

factor :: Parser Char
factor (n,ns,xs)
  | rs1 == []        = (n1,m1,rs1)
  | isAst (head rs1) = (n1,closure m1,tail rs1)
  | isPls (head rs1) = (n1,positive m1,tail rs1)
  | otherwise        = (n1,m1,rs1)
    where
    (n1,m1,rs1) = primary (n,ns,xs)

prediclass :: (Char -> Bool) -> [Int] -> Match Char
prediclass _ _  (_,[]) = []
prediclass p ns (gs,x:xs)
  | p x       = [(grpadd ns gs x,xs)]
  | otherwise = []

char :: Char -> [Int] -> Match Char
char c = prediclass (c==)

anyascii :: [Int] -> Match Char
anyascii ns = prediclass (<='\x7F') ns

is8bit :: Char -> Bool
is8bit x = '\xA1' <= x && x <= '\xFE'

any8bit :: [Int] -> Match Char
any8bit ns = prediclass is8bit ns

anymultibyte :: [Int] -> Match Char
anymultibyte ns =
  uni (cat (any8bit ns) (any8bit ns))
      (uni (cat (char '\x8E' ns) (any8bit ns))
           (cat (char '\x8F' ns) (cat (any8bit ns) (any8bit ns))))

str2uni :: [String] -> String
str2uni cs = foldl1 (\s t -> s ++ "|" ++ t) cs

charclass :: Parser Char
charclass (n,ns,xs)
  | isPrefixOf "[:ascii:]"     xs = mkclass0 "[:ascii:]"     anyascii
  | isPrefixOf "[:multibyte:]" xs = mkclass0 "[:multibyte:]" anymultibyte
  | isPrefixOf "[:digit:]"     xs = mkclass1 "[:digit:]"     isDigit
  | isPrefixOf "[:alpha:]"     xs = mkclass1 "[:alpha:]"     isAlpha
  | isPrefixOf "[:fullalpha:]" xs = mkclass2 "[:fullalpha:]" fullalpha
  | isPrefixOf "[:fulldigit:]" xs = mkclass2 "[:fulldigit:]" fulldigit
  | isPrefixOf "[:fullalnum:]" xs = mkclass2 "[:fullalnum:]" fullalnum
  | isPrefixOf "[:hiragana0:]" xs = mkclass2 "[:hiragana0:]" hiragana0
  | isPrefixOf "[:hiragana:]"  xs = mkclass2 "[:hiragana:]"  hiragana
  | isPrefixOf "[:katakana0:]" xs = mkclass2 "[:katakana0:]" katakana0
  | isPrefixOf "[:katakana:]"  xs = mkclass2 "[:katakana:]"  katakana
  | otherwise                     = syntaxerror
    where
    mkclass0 name m = (n,m ns,drop (length name) xs)
    mkclass1 name p = (n,prediclass p ns,drop (length name) xs)
    mkclass2 name cs = (n,m,drop (length name) xs)
                       where (_,m,_) = expr (n,ns,str2uni cs)

literal :: Parser Char
literal (_,_ ,[]) = syntaxerror
literal (n,ns,(x:xs))
  | (<='\x7F') x =
      (n,char x ns,xs)
  | is8bit x && is8bit y =
      (n,cat (char x ns) (char y ns),ys)
  | '\x8E' == x && is8bit y =
      (n,cat (char x ns) (char y ns),ys)
  | '\x8F' == x && is8bit y && is8bit z =
    (n,cat (cat (char x ns) (char y ns)) (char z ns),zs)
  | otherwise =
    (n,char x ns,xs)
    where
    (y:ys) = xs; (z:zs) = ys

primary :: Parser Char
primary (_,_,[]) = syntaxerror
primary (n,ns,xxs@(x:xs))
  | isDot  x  = (n,uni (anyascii ns) (anymultibyte ns),xs)
  | isLPar x  = if rs1 /= [] && isRPar (head rs1)
                then (n1,m1,tail rs1)
                else syntaxerror
  | isLBrk x  = charclass (n,ns,xxs)
  | otherwise = literal (n,ns,xxs)
    where
    (n1,m1,rs1) = expr (n+1,ns++[n+1],xs)

regex :: String -> String -> [String]
regex _ [] = []
regex p xs = if r == []
             then []
             else if v == []
                  then u
                  else []
             where
             (n,m) = parse p
             r = m ((replicate (n+1) []),xs)
             (u,v) = head r

prefixAny :: (Int,Match Char) -> String -> [(String,String)]
prefixAny (_,_) [] = []
prefixAny (n,m) xs = if r == [] then [] else [(head u,v)]
                     where
                     r = m ((replicate (n+1) []),xs)
                     (u,v) = head r

prefixAscii :: String -> [(String,String)]
prefixAscii     = prefixAny (parse "([:ascii:][:ascii:]*)")

prefixMultibyte :: String -> [(String,String)]
prefixMultibyte = prefixAny (parse "([:multibyte:][:multibyte:]*)")

prefixDigit :: String -> [(String,String)]
prefixDigit     = prefixAny (parse "([:digit:][:digit:]*)")

prefixAlpha :: String -> [(String,String)]
prefixAlpha     = prefixAny (parse "([:alpha:][:alpha:]*)")

prefixFullDigit :: String -> [(String,String)]
prefixFullDigit = prefixAny (parse "([:fulldigit:][:fulldigit:]*)")

prefixFullAlpha :: String -> [(String,String)]
prefixFullAlpha = prefixAny (parse "([:fullalpha:][:fullalpha:]*)")

prefixHiragana :: String -> [(String,String)]
prefixHiragana  = prefixAny (parse "([:hiragana:][:hiragana:]*)")

prefixKatakana :: String -> [(String,String)]
prefixKatakana  = prefixAny (parse "([:katakana:][:katakana:]*)")

yomiNotBad :: String -> Bool
yomiNotBad x = (not.null) m
               where
               p ="[:hiragana0:][:hiragana:][:hiragana:]*"
               m = prefixAny (parse p) x

split :: String -> [String]
split [] = []
split (x:xs)  | x <= '\x7F'               = [x]     : split xs
              | b x && b y                = [x,y]   : split ys
              | x == '\x8E' && b y        = [x,y]   : split ys
              | x == '\x8F' && b y && b z = [x,y,z] : split zs
              | otherwise                 = [x]     : split xs
                where
                (y:ys) = xs; (z:zs) = ys
                b c = '\xA1' <= c && c <= '\xFE'

mblen :: String -> Int
mblen = length.split

replace1 :: [(String,String)] -> String -> (String,String)
replace1 []         y = ("",y)
replace1 ((s,t):xs) y | isPrefixOf s y = (t,drop (length s) y)
                      | otherwise      = replace1 xs y

replace0 :: [(String,String)] -> String -> String
replace0 _ [] = []
replace0 p xs = let (y,ys) = replace1 p xs in
                  if y == ""
                  then let x = head (split xs) in
                         x ++ (replace0 p (drop (length x) xs))
                  else y ++ (replace0 p ys)

hira2kata :: String -> String
hira2kata = replace0 ([("¤¦¡«","¥ô")] ++ (zip hiragana katakana))

kata2hira :: String -> String
kata2hira = replace0 ([("¥ô","¤¦¡«")] ++ (zip katakana hiragana))

fulldigit,fullalpha,fullalnum :: [String]
hiragana0,katakana0,hiragana,katakana :: [String]

fulldigit = ["£°","£±","£²","£³","£´","£µ","£¶","£·","£¸","£¹"]
fullalpha = ["£Á","£Â","£Ã","£Ä","£Å","£Æ","£Ç","£È","£É","£Ê","£Ë","£Ì","£Í",
             "£Î","£Ï","£Ð","£Ñ","£Ò","£Ó","£Ô","£Õ","£Ö","£×","£Ø","£Ù","£Ú",
             "£á","£â","£ã","£ä","£å","£æ","£ç","£è","£é","£ê","£ë","£ì","£í",
             "£î","£ï","£ð","£ñ","£ò","£ó","£ô","£õ","£ö","£÷","£ø","£ù","£ú"]

fullalnum = fulldigit ++ fullalpha

hiragana0 = ["¤¢","¤¤","¤¦","¤¨","¤ª",
             "¤«","¤­","¤¯","¤±","¤³",
             "¤µ","¤·","¤¹","¤»","¤½",
             "¤¿","¤Á","¤Ä","¤Æ","¤È",
             "¤Ê","¤Ë","¤Ì","¤Í","¤Î",
             "¤Ï","¤Ò","¤Õ","¤Ø","¤Û",
             "¤Þ","¤ß","¤à","¤á","¤â",
             "¤ä","¤æ","¤è",
             "¤é","¤ê","¤ë","¤ì","¤í",
             "¤ï","¤ð","¤ñ","¤ò",
             "¤¬","¤®","¤°","¤²","¤´",
             "¤¶","¤¸","¤º","¤¼","¤¾",
             "¤À","¤Â","¤Å","¤Ç","¤É",
             "¤Ð","¤Ó","¤Ö","¤Ù","¤Ü",
             "¤Ñ","¤Ô","¤×","¤Ú","¤Ý"]

katakana0 = ["¥¢","¥¤","¥¦","¥¨","¥ª",
             "¥«","¥­","¥¯","¥±","¥³",
             "¥µ","¥·","¥¹","¥»","¥½",
             "¥¿","¥Á","¥Ä","¥Æ","¥È",
             "¥Ê","¥Ë","¥Ì","¥Í","¥Î",
             "¥Ï","¥Ò","¥Õ","¥Ø","¥Û",
             "¥Þ","¥ß","¥à","¥á","¥â",
             "¥ä","¥æ","¥è",
             "¥é","¥ê","¥ë","¥ì","¥í",
             "¥ï","¥ð","¥ñ","¥ò",
             "¥¬","¥®","¥°","¥²","¥´",
             "¥¶","¥¸","¥º","¥¼","¥¾",
             "¥À","¥Â","¥Å","¥Ç","¥É",
             "¥Ð","¥Ó","¥Ö","¥Ù","¥Ü",
             "¥Ñ","¥Ô","¥×","¥Ú","¥Ý"]

hiragana  = hiragana0 ++
            ["¤ó",
             "¤¡","¤£","¤¥","¤§","¤©",
             "¤Ã",
             "¤ã","¤å","¤ç",
             "¤î",
             "¡¼"]

katakana  = katakana0 ++
            ["¥ó",
             "¥¡","¥£","¥¥","¥§","¥©",
             "¥Ã",
             "¥ã","¥å","¥ç",
             "¥î",
             "¡¼"]

han2zen :: String -> String
han2zen = replace0 hanzen

zen2han :: String -> String
zen2han = replace0 (map (\(x,y) -> (y,x)) hanzen)

hanzen :: [(String,String)]
hanzen = [("!","¡ª"),("\"","¡É"),("#","¡ô"),("$","¡ð"),("%","¡ó"),("&","¡õ"),
          ("'","¡Ç"),("(","¡Ê"),(")","¡Ë"),("*","¡ö"),("+","¡Ü"),(",","¡¤"),
          ("-","¡Ý"),(".","¡¥"),("/","¡¿"),
          ("0","£°"),("1","£±"),("2","£²"),("3","£³"),("4","£´"),("5","£µ"),
          ("6","£¶"),("7","£·"),("8","£¸"),("9","£¹"),(":","¡§"),(";","¡¨"),
          ("<","¡ã"),("=","¡á"),(">","¡ä"),("?","¡©"),("@","¡÷"),
          ("A","£Á"),("B","£Â"),("C","£Ã"),("D","£Ä"),("E","£Å"),("F","£Æ"),
          ("G","£Ç"),("H","£È"),("I","£É"),("J","£Ê"),("K","£Ë"),("L","£Ì"),
          ("M","£Í"),("N","£Î"),("O","£Ï"),("P","£Ð"),("Q","£Ñ"),("R","£Ò"),
          ("S","£Ó"),("T","£Ô"),("U","£Õ"),("V","£Ö"),("W","£×"),("X","£Ø"),
          ("Y","£Ù"),("Z","£Ú"),
          ("[","¡Î"),("\\","¡ï"),("]","¡Ï"),("^","¡°"),("_","¡²"),("`","¡®"),
          ("a","£á"),("b","£â"),("c","£ã"),("d","£ä"),("e","£å"),("f","£æ"),
          ("g","£ç"),("h","£è"),("i","£é"),("j","£ê"),("k","£ë"),("l","£ì"),
          ("m","£í"),("n","£î"),("o","£ï"),("p","£ð"),("q","£ñ"),("r","£ò"),
          ("s","£ó"),("t","£ô"),("u","£õ"),("v","£ö"),("w","£÷"),("x","£ø"),
          ("y","£ù"),("z","£ú"),
          ("{","¡Ð"),("|","¡Ã"),("}","¡Ñ"),("~","¡±")]
