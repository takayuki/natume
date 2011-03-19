{--
 --  Natume -- an implementation of Kana-Kanji conversion in Haskell
 --  Copyright (C) 2006 2007 Takayuki Usui
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
module Session (
    Session,session_init,session_free,
    beginConvert,endConvert,
    getCandidates,resizePause,updateContext
  ) where

import Prelude hiding (id)
import Convert
import Con
import Dic
import Re
import qualified Config
--import Compile (rensetu_tbl1,rensetu_tbl2)
--import qualified Rensetu

type Pause = (Mrph,[DicWord])
type MetaPause = [Pause]
type Context = (Int,[MetaPause])
type Session = (Con,[Dic],[Context])

ro :: String -> String
ro x = Config.nadicdir ++ Config.pathsep ++ x

ipadic,extdic,usrdic,tmpdic :: (String,String,String)
ipadic = (ro "ipadic.idx",ro "ipadic.dat","ipadic.sta")
extdic = (ro "extdic.idx",ro "extdic.dat","extdic.sta")
usrdic = ("user.idx","user.dat","user.sta")
tmpdic = ("tmpdic.idx","tmpdic.dat","tmpdic.sta")

session_init :: IO Session
session_init = do con <- connect_init (ro "connect.idx") (ro "connect.dat")
                  dics <- initDics [ipadic,extdic,usrdic,tmpdic]
                  return (con,dics,[(0,[])])

session_free :: Session -> IO ()
session_free ssn = do let (con,dics,_) = ssn
                      connect_free con
                      freeDics dics
                      return ()

updateContext :: Int -> Context -> [Context] -> [Context]
updateContext _ _   []     = []
updateContext 0 cxt (_:xs) = cxt : xs
updateContext n cxt (x:xs) = x : (updateContext (n-1) cxt xs)

updatePause0 :: Int -> MetaPause -> [MetaPause] -> [MetaPause]
updatePause0 _ _     []     = []
updatePause0 0 p (_:xs) = p : xs
updatePause0 n p (x:xs) = x : (updatePause0 (n-1) p xs)

updatePause :: Int -> MetaPause -> Context -> Context
updatePause n p (mode,pause) = (mode,updatePause0 n p pause)

chooseStyle :: Mrph -> DicWord
chooseStyle mrph =
  case mrph of
    (MkMrph _ w _ _ d p 0 _) -> (w,d,p,0)
    (MkMrph y _ _ _ d p 1 _) -> (y,d,p,1)
    (MkMrph y _ _ _ d p 2 _) -> (hira2kata y,d,p,2)
    (MkMrph y _ _ _ d p 3 _) -> (zen2han y,d,p,3)
    (MkMrph _ w _ _ d p s _) -> (w,d,p,s)

modal :: Int -> Int -> Int
modal a 0 = a
modal a x =
  let c = x `mod` 16
      d = if c <= 4 then c else 0 in
    modal ((a * 16) + d) (x `div` 16)

{-
rensetu :: [String] -> [Int]
rensetu name = map Rensetu.index
                   (filter (Rensetu.match (name,"","",""))
                           (rensetu_tbl1 ++ rensetu_tbl2))

body,part :: [Int]
body = concatMap rensetu [["Ì¾»ì"],["Æ°»ì"],["·ÁÍÆ»ì"],["Éû»ì"]]
part = concatMap rensetu [["½õ»ì"],["½õÆ°»ì"]]

isBody :: Mrph -> Bool
isBody (MkMrph _ _ t _ _ _ _ _) = elem t body

isPart :: Mrph -> Bool
isPart (MkMrph _ _ t _ _ _ _ _) = elem t part

group0 :: [Pause] -> [MetaPause] -> [MetaPause]
group0 []     ys     = if null (head ys) then (tail ys) else ys
group0 (x:xs) []     = group0 (x:xs) [[]]
group0 (x:xs) (y:ys) =
  let (mrph,_) = x in
    if isPart mrph
    then group0 xs ((x:y):ys)
    else if isBody mrph
         then if null y
              then group0 xs ([]:[x]:ys)
              else group0 xs ([]:(x:y):ys)
         else if null y
              then group0 xs ([]:[x]:ys)
              else group0 xs ([]:[x]:y:ys)

group :: [Pause] -> [MetaPause]
group x = group0 (reverse x) [[]]
-}

group :: [Pause] -> [MetaPause]
group x = map (:[]) x

beginConvert :: Session -> Int -> String -> Int -> IO Session
beginConvert ssn cont input mode =
  do let (con,dics,cxts) = ssn
     let mode0 = modal 0 mode
     let mode1 = if mode0 == 0 then 19 else mode0
     mrphs <- convert con dics input
     let cxt = (mode1,group (zip mrphs (map ((:[]).chooseStyle) mrphs)))
     return (con,dics,updateContext cont cxt cxts)

endConvert :: Session -> Int -> [Int] -> IO Session
endConvert ssn cont _ =
  do let (con,dics,cxts) = ssn
     return (con,dics,updateContext cont (0,[]) cxts)

getCandidates :: Session -> Int -> Int -> IO Session
getCandidates ssn cont idx =
  do let (con,dics,cxts) = ssn
     let (cxt@(mode,pause)) = cxts !! cont
     let ((mrph,_):parts) = pause !! idx
     cnds <- candidate dics mrph mode
     let cxt' = updatePause idx ((mrph,cnds):parts) cxt
     return (con,dics,updateContext cont cxt' cxts)

reconvert :: Session -> Pause -> IO MetaPause
reconvert (_,dics,_) p =
  let (mrph,_) = p in
   case mrph of
     (MkMrph key _ 0 0 0 0 0 0) ->
       do mrphs <- lookupDics dics key
          if null mrphs
            then return [(MkMrph key key glue 0 0 0 1 0,[(key,0,0,1)])]
            else return [(head mrphs,[chooseStyle (head mrphs)])]
     _ -> return [p]

yomi :: Mrph -> String
yomi (MkMrph y _ _ _ _ _ _ _) = y

reconvert_ :: Session -> MetaPause -> [MetaPause] -> IO [MetaPause]
reconvert_ _            []           _ = return []
reconvert_ (con,dics,_) ((mrph,_):_) p =
  do let input = concatMap (concatMap (yomi.fst)) p
     mrphs <- convert_ con dics mrph input
     return (group (zip mrphs (map ((:[]).chooseStyle) mrphs)))

skel :: String -> Mrph
skel x = MkMrph x x 0 0 0 0 0 0

join :: MetaPause -> Pause
join x = (skel w',[])
         where
         w' = concat (map (\(MkMrph w _ _ _ _ _ _ _,_) -> w) x)

enlarge :: Session -> Pause -> Int -> [MetaPause] -> IO [MetaPause]
enlarge ssn p 0 []     = mapM (reconvert ssn) [p]
enlarge ssn p 0 (q:qs) =
  do r <- reconvert ssn p
     rs <- reconvert_ ssn r (q:qs)
     return (r:rs)
enlarge ssn p n [] =
  let (mrph,_) = p
      (MkMrph x _ _ _ _ _ _ _) = mrph in
    let xs = split x
        m0 = skel (head xs) in
      if (length xs) == 1  
      then enlarge ssn (m0,[]) 0 []
      else let m1 = skel (concat (tail xs)) in
             enlarge ssn (m0,[]) (n-1) [[(m1,[])]]
enlarge ssn p n (q:qs) =
  let (mrph,_) = p
      (MkMrph x _ _ _ _ _ _ _) = mrph in
    let (m,_) = join q
        (MkMrph y _ _ _ _ _ _ _) = m in
      let ys = split y
          m0 = skel (x ++ (head ys)) in
        if (length ys) == 1
        then enlarge ssn (m0,[]) (n-1) qs
        else let m1 = skel (concat (tail ys)) in
               enlarge ssn (m0,[]) (n-1) ([(m1,[])] : qs)

shorten :: Session -> Pause -> Int -> [MetaPause] -> IO [MetaPause]
shorten ssn p 0 []     = mapM (reconvert ssn) [p]
shorten ssn p 0 (q:qs) =
  do r <- reconvert ssn p
     rs <- reconvert_ ssn r (q:qs)
     return (r:rs)
shorten ssn p n [] =
  let (mrph,_) = p
      (MkMrph x _ _ _ _ _ _ _) = mrph in
    let xs = split x
        m0 = skel (concat (init xs))
        m1 = skel (last xs) in
      if (length xs) == 1  
      then shorten ssn (m1,[]) 0 []
      else shorten ssn (m0,[]) (n-1) [[(m1,[])]]
shorten ssn p n (q:qs) =
  let (mrph,_) = p
      (MkMrph x _ _ _ _ _ _ _) = mrph in
    let (m,_) = join q
        (MkMrph y _ _ _ _ _ _ _) = m in
      let xs = split x
          m0 = skel (concat (init xs))
          m1 = skel ((last xs) ++ y) in
        if (length xs) == 1  
        then shorten ssn (m1,[]) 0 qs
        else shorten ssn (m0,[]) (n-1) ([(m1,[])] : qs)

resize0 :: Session -> Int -> [MetaPause] -> IO [MetaPause]
resize0 _   _    []     = return []
resize0 ssn size (x:xs) = 
  let (mrph,_) = join x in
    let (MkMrph y _ _ _ _ _ _ _) = mrph in
      let base = length (split y) in
        if size == base
        then return (x:xs)
        else if base < size
             then enlarge ssn (mrph,[]) (size-base) xs
             else shorten ssn (mrph,[]) (base-size) xs

resize ::  Session -> [MetaPause] -> Int -> Int -> IO [MetaPause]
resize _    []     _ _    = return []
resize ssn (x:xs) 0 size = resize0 ssn size (x:xs)
resize ssn (x:xs) n size = do xs' <- resize ssn xs (n-1) size
                              return (x:xs')

resizePause :: Session -> Int -> Int -> Int -> IO Session
resizePause ssn cont idx size =
  do let (con,dics,cxts) = ssn
     let (mode,pause) = cxts !! cont
     pause' <- resize ssn pause idx size
     return (con,dics,updateContext cont (mode,pause') cxts)

