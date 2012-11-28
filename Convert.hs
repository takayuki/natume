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
module Convert (
    initDics,freeDics,lookupDics,convert,convert_,candidate
  ) where

import Prelude hiding (id,last)
import qualified Data.List
import Con
import Dic
import Re

undefword_cost,undefconnect_cost :: Int
undefword_cost = 10000
undefconnect_cost = 10000

table :: Mrph -> Int
table (MkMrph _ _ t _ _ _ _ _) = t

cost :: Mrph -> Int
cost (MkMrph _ _ _ c _ _ _ _) = c

dict :: Mrph -> Int
dict (MkMrph _ _ _ _ d _ _ _) = d

point :: Mrph -> Int
point (MkMrph _ _ _ _ _ p _ _) = p

last :: Mrph -> Int
last (MkMrph _ _ _ _ _ _ _ m) = m

initDics :: [(String,String,String)] -> IO [Dic]
initDics []     = return []
initDics (x:xs) = let (idx,dat,sta) = x in
                    do d <- dic_init idx dat sta
                       if null d
                         then initDics xs
                         else do ds <- initDics xs
                                 return ((head d):ds)

freeDics :: [Dic] -> IO ()
freeDics []     = return ()
freeDics (x:xs) = do dic_free x
                     freeDics xs

orderByUsage :: Mrph -> Mrph -> Ordering
orderByUsage (MkMrph _ _ _ c1 _ _ _ m1) (MkMrph _ _ _ c2 _ _ _ m2)
       | m1 < m2   = LT
       | m1 > m2   = GT
       | c1 < c2   = LT
       | c1 > c2   = GT
       | otherwise = EQ

orderByTable :: Mrph -> Mrph -> Ordering
orderByTable (MkMrph y1 _ t1 _ _ _ _ _) (MkMrph y2 _ t2 _ _ _ _ _)
       | y1 < y2   = LT
       | y1 > y2   = GT
       | t1 < t2   = LT
       | t1 > t2   = GT
       | otherwise = EQ

cmpByTable :: Mrph -> Mrph -> Bool
cmpByTable m1 m2 = case orderByTable m1 m2 of
                     EQ -> True
                     _  -> False

mergeDics :: (Mrph -> Mrph -> Ordering) -> [[Mrph]] -> [Mrph]
mergeDics _ []  = []
mergeDics _ [x] = x
mergeDics order ([]:[]:xs) = mergeDics order xs
mergeDics order ([]:x2:xs) = x2 ++ mergeDics order xs
mergeDics order (x1:[]:xs) = x1 ++ mergeDics order xs
mergeDics order ((x1:xs1):(x2:xs2):xs) =
  case (order x1 x2) of
    GT  -> (x2:(mergeDics order ((x1:xs1):xs2:xs)))
    LT  -> (x1:(mergeDics order (xs1:(x2:xs2):xs)))
    _   -> (x1:x2:(mergeDics order (xs1:xs2:xs)))


lookupDics0 :: [Dic] -> String -> Int -> IO [[Mrph]]
lookupDics0 []     _   _ = return []
lookupDics0 (x:xs) key exact = do h <- dic_search x key exact
                                  hs <- lookupDics0 xs key exact
                                  return (h:hs)

lookupDics :: [Dic] -> String -> IO [Mrph]
lookupDics xs key =
  do hs <- lookupDics0 xs key 1
     return (mergeDics orderByUsage
                       (map (Data.List.sortBy orderByUsage) hs))

group :: [[Mrph]] -> [Mrph]
group xs = map (head.(Data.List.sortBy orderByUsage))
               (Data.List.groupBy cmpByTable (mergeDics orderByTable xs))

contractDics :: [Dic] -> String -> IO [Mrph]
contractDics xs key =
  do hs <- lookupDics0 xs key 0
     return (Data.List.sortBy orderByUsage (group hs))


tails0 :: Int -> [[a]] -> [(Int,[[a]])]
tails0 n []         =  [(n,[])]
tails0 n xxs@(x:xs) = (n,xxs) : tails0 (n + (length x)) xs

tails :: String -> [(Int,String)]
tails x = map (\(n,s) -> (n,concat s)) (tails0 0 (split x))

type Stop = (Mrph,Int,Int)
type Path = [Stop]
type Start = (Int,String,[Path])

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,x) = x

initstate :: Mrph -> String -> [Start]
initstate start input =
  zip3 (map fst ts) (map snd ts) ([[[(start,0,0)]]] ++ repeat [])
  where
  ts = tails input

chooseRule :: Path -> [([Int],Int)] -> (Int,Int) -> (Int,Int)
chooseRule []   _           _    = error "Convert.chooseRule: empty path"
chooseRule _    []          best = best
chooseRule _    (([],_):_)  _    = error "Convert.chooseRule: empty rule"
chooseRule _    (([_],_):_) _    = error "Convert.chooseRule: singleton rule"
chooseRule path ((rule,connect):ys) (bestbase,bestconnect) =
  if (base + connect) < (bestbase + bestconnect)
  then chooseRule path ys (base,connect)
  else chooseRule path ys (bestbase,bestconnect)
  where
  gram = length rule
  base = (snd3 (head path)) - foldl (\a b -> a + (thd3 b))
                                    0 (take (gram - 2) (tail path))

choosePath :: Con -> Mrph -> [Path] -> (Int,Path) -> IO Path
choosePath _ _ [] (_,bestpath) = return bestpath
choosePath con startmrph (x:xs) (bestcost,bestpath) =
  let ((stopmrph,sofar,_):behind) = x
      backtrace = map table (startmrph : stopmrph : (map fst3 behind))
    in
      do cons <- connect_search con backtrace
         let (base,connect) =
               if null cons
               then (sofar,undefconnect_cost)
               else chooseRule x cons (536870912,536870912)
         if (base+connect) < bestcost
           then
             let lst = fromIntegral (last startmrph)
                 weight = (1.0::Double) - exp ((-2.0) * (lst / 2678400.0))
                 wordcost = round ((fromIntegral (cost startmrph)) * weight)
             in
               choosePath con startmrph xs
               (base+connect,
                ((startmrph,base+connect+wordcost,undefconnect_cost):
                 (stopmrph,base,connect):behind))
           else choosePath con startmrph xs
                (bestcost,bestpath)

sortpath :: [Path] -> [Path]
sortpath = Data.List.sortBy cmp
           where
           cmp x y = compare (x1+x2) (y1+y2) 
                     where
                     (_,x1,x2) = head x; (_,y1,y2) = head y

extend :: [Start] -> IO [Start]
extend []    = error "Convert.extent: empty state"
extend state =
  let (pos,input,paths) = head state in
    if null paths
    then return state
    else
      do let yomi = head (split input)
         let mrph = MkMrph yomi yomi glue undefword_cost 0 0 0 0
         let behind = head (sortpath paths)
         let (_,base,connect) = head behind
         let word = (mrph,base+connect+undefword_cost,undefconnect_cost)
         let path = word : behind
         return (newword pos path state)

choose :: Con -> [Mrph] -> [Start] -> IO [Start]
choose _   []     state = return state
choose con (x:xs) state =
 let (pos,_,paths) = head state in
   if null paths
   then return state
   else do bestpath <- choosePath con x paths (536870912,[])
           choose con xs (newword pos bestpath state)

newword0 :: Int -> [Stop] -> [Start] -> [Start]
newword0 _   _    []     = []
newword0 pos path (x:xs) =
  let (p,str,paths) = x in
    if p == pos then (pos,str,path:paths) : xs
                else x : (newword0 pos path xs)

newword :: Int -> [Stop] -> [Start] -> [Start]
newword pos path state =
  newword0 (pos + (length y)) path state
  where
  (MkMrph y _ _ _ _ _ _ _) = fst3 (head path)

skip0 :: Int -> [Start] -> [Start]
skip0 _   []     = []
skip0 pos (x:xs) =
  let (p,_,_) = x in
    if p == pos then x : xs
                else skip0 pos xs

skip :: Int -> [Stop] -> [Start] -> [Start]
skip pos path state =
  skip0 (pos + (length y)) state
  where
  (MkMrph y _ _ _ _ _ _ _) = fst3 (head path)

type Conv = Con -> [Dic] -> [Start] -> IO [Start]

convert9 :: Conv
convert9 _   _    []  = error "Convert.convert9: empty state"
convert9 _   _    [x] = return [x]
convert9 con dics state =
  let (_,input,paths) = head state in
    if null paths
    then convert0 con dics (tail state)
    else
      do mrphs <- contractDics dics input
         if null mrphs
           then do state' <- extend state
                   convert0 con dics (tail state')
           else do state' <- choose con mrphs state
                   convert0 con dics (tail state')

mkconv :: (String -> [(String,String)]) -> Int -> Conv -> Conv
mkconv _     _   _    _   _    []  = error "Convert.mkconv: empty state"
mkconv _     _   _    _   _    [x] = return [x]
mkconv match tbl next con dics state =
  let (pos,input,paths) = head state
      prefix = match input
  in
    if null paths || null prefix
    then
      next con dics state
    else
      do let yomi = (fst.head) prefix
         let undef = MkMrph yomi yomi tbl undefword_cost 0 0 0 0
         let behind = head (sortpath paths)
         let (_,base,connect) = head behind
         let word = (undef,base+connect+undefword_cost,undefconnect_cost)
         let path = word : behind
         convert0 con dics (skip pos path (newword pos path state))

convert8 :: Conv
convert8 = mkconv prefixKatakana strange convert9

convert7 :: Conv
convert7 = convert8

convert6 :: Conv
convert6 = convert7

convert5 :: Conv
convert5 = mkconv prefixAscii strange convert6

convert4 :: Conv
convert4 = mkconv prefixFullAlpha strange convert5

convert3 :: Conv
convert3 = mkconv prefixAlpha strange convert4

convert2 :: Conv
convert2 = mkconv prefixFullDigit number convert3

convert1 :: Conv
convert1 = mkconv prefixDigit number convert2

convert0 :: Conv
convert0 = convert1

adhoc :: [Path] -> [Path]
adhoc []         = []
adhoc [p]        = [p]
adhoc (p1:p2:ps) = if (length p1) <= (3+1) && (length p1) > (length p2)
                   then (p2:p1:ps)
                   else (p1:p2:ps)
{-
showpath :: Path -> IO ()
showpath path =
       do mapM_ (\((MkMrph _ w _ _ _ _ _ _),c1,c2) ->
                  putStr (w ++ " (" ++ (show c1) ++ "," ++ (show c2) ++ ") ")
                 ) (reverse path)
          putStr "\n"
-}

convert_ :: Con -> [Dic] -> Mrph -> String -> IO [Mrph]
convert_ con dics mrph input =
  do ((_,_,stps):_) <- convert0 con dics (initstate mrph input)
     if (length stps) == 0
       then return []
       else do let bestpath = head (adhoc (sortpath (reverse stps)))
               --mapM_ showpath (adhoc (sortpath (reverse stps)))
               return (map fst3 ((reverse.init) bestpath))

convert :: Con -> [Dic] -> String -> IO [Mrph]
convert con dics input = convert_ con dics bos input

enum_style :: Mrph -> [DicWord]
enum_style mrph =
  case mrph of
{-
    (MkMrph y w _ _ d p 1 _) ->
      [(y,d,p,1),(hira2kata y,d,p,2),(zen2han y,d,p,3),(w,d,p,0)]
    (MkMrph y w _ _ d p 2 _) ->
      [(hira2kata y,d,p,2),(zen2han y,d,p,3),(w,d,p,0),(y,d,p,1)]
    (MkMrph y w _ _ d p 3 _) ->
      [(zen2han y,d,p,3),(w,d,p,0),(y,d,p,1),(hira2kata y,d,p,2)]
    (MkMrph y w _ _ d p _ _) ->
      [(w,d,p,0),(y,d,p,1),(hira2kata y,d,p,2),(zen2han y,d,p,3)]
-}      
    (MkMrph y w _ _ d p 1 _) ->
      [(y,d,p,1),(w,d,p,0)]
    (MkMrph y w _ _ d p 2 _) ->
      [(hira2kata y,d,p,2),(w,d,p,0)]
    (MkMrph y w _ _ d p 3 _) ->
      [(zen2han y,d,p,3),(w,d,p,0)]
    (MkMrph _ w _ _ d p s _) ->
      [(w,d,p,s)]

modal :: Int -> (String,Int,Int) -> [DicWord]
modal 0 _ = []
modal x (key,dic,pnt) =
  let m = case (x `mod` 16) of
            0 -> [(key,dic,pnt,1)]
            1 -> [(key,dic,pnt,1)]
            2 -> [(zen2han key,dic,pnt,3)]
            3 -> [(hira2kata key,dic,pnt,2)]
            _ -> []
  in
    m ++ (modal (x `div` 16) (key,dic,pnt))

dicword :: Mrph -> DicWord
dicword (MkMrph _ w _ _ d p _ _) =(w,d,p,0)

candidate :: [Dic] -> Mrph -> Int -> IO [DicWord]
candidate dics mrph mode =
  do let (MkMrph key _ _ _ dic pnt _ _) = mrph
     mrphs <- lookupDics dics key
     let mrph' = enum_style mrph
     let mrphs' = filter (\x -> not (ident (dic,pnt) x)) mrphs
     let dicwords = (uniq1 mrph') ++ (map dicword mrphs')
     let modalwords = modal mode (key,dic,pnt)
     return ((uniq2 dicwords) ++ modalwords)
  where
  ident (d,p) x = (dict x) == d && (point x) == p
  uniq1 = Data.List.nubBy (\(w1,_,_,_) (w2,_,_,_) -> w1 == w2)
  uniq2 = Data.List.nubBy (\(w1,_,_,s1) (w2,_,_,s2) -> w1 == w2 && s1 == s2)

