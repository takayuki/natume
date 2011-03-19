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
module Rensetu (
    Rensetu(MkRensetu),Ren,
    index,hinsi,ktype,kform,goi,
    match,mktab,decode,compile,convert,
  ) where

import Prelude hiding (id)
import qualified Connect
import qualified Hinsi
import qualified KForm
import qualified KType

data Rensetu = MkRensetu { index  :: Int,
                           hinsi  :: Hinsi.Hinsi,
                           ktype  :: [KType.KType],
                           kform  :: [KForm.KForm],
                           goi   :: String }
               deriving (Show)

type Ren = (Int,Int,Int,Int,String)

allForms :: KType.KType -> [(KType.KType,KForm.KForm)]
allForms x = map (\f -> (x,f)) (KType.kform x)

allTypes :: Hinsi.Hinsi -> [(KType.KType,KForm.KForm)]
allTypes x = concat (map (\t -> (allForms t)) (Hinsi.ktype x))

bare :: [Hinsi.Hinsi] -> [Rensetu]
bare []     = []
bare (x:xs) = if (Hinsi.kt x)
              then (map (\(t,f) -> MkRensetu 0 x [t] [f] [])
                           (allTypes x)) ++ (bare xs)
              else (MkRensetu 0 x [] [] []) : bare xs

withGoi :: [Connect.Rule] -> [([String],String,String,String)]
withGoi []     = []
withGoi (x:xs) = (filter hasgoi x) ++ (withGoi xs)
                  where
                  hasgoi (_,_,_,g) = not (null g)

match :: ([String],String,String,String) -> Rensetu -> Bool
match x r =
  let (h1,t1,f1,g1) = x
      (MkRensetu _ h2 t2 f2 g2) = r in
    and [Hinsi.isprefixof h1 (Hinsi.name h2),
         or [(t1 == []),((not (null t2)) && (t1 == (KType.name (head t2))))],
         or [(f1 == []),((not (null f2)) && (f1 == (KForm.name (head f2))))],
         or [(g1 == []),((not (null g2)) && (g1 == g2))]]

update :: Connect.Rule ->([Rensetu],[Rensetu]) -> [Rensetu]
update []     (rs,_)  = rs
update (x:xs) (rs,es) =
  if not (null (filter (match x) (rs ++ es)))
  then update xs (rs,es)
  else if null t
       then update xs ((r : rs),es)
       else update xs (((reverse rs') ++ rs),es)
       where
       (h,t,f,g) = x
       r = (head (filter (match (h,t,f,"")) (rs ++ es))) {goi=g}
       rs' = map (\ren -> ren {goi=g}) (filter (match (h,t,"","")) es)

enum0 :: Int -> [Rensetu] -> (Int,[Rensetu])
enum0 n []     = (n,[])
enum0 n (x:xs) = (n1,x1:xs1)
                 where
                 (n1,xs1) = enum0 (n+1) xs
                 x1 = x {index=n}

enum :: [Rensetu] -> [Rensetu]
enum xs = snd (enum0 0 xs)

mktab :: [Hinsi.Hinsi] -> [Connect.Connect] -> ([Rensetu],[Rensetu])
mktab hs cs = ((take half ss),(drop half ss))
              where
              es = bare hs
              fs = withGoi (map fst cs)
              gs = update fs ([],(tail es))
              rs = (head es) : ((reverse gs) ++ (tail es))
              ss = enum rs
              half = (length gs) + 1

encode :: [Rensetu] -> [Ren]
encode []     = []
encode (x:xs) =
  (i1,h1,t1,f1,g1) : encode xs
  where
  (MkRensetu i0 h0 t0 f0 g0) = x
  i1 = i0
  h1 = Hinsi.index h0
  t1 = if null t0 then 0 else (KType.index (head t0))
  f1 = if null f0 then 0 else (KForm.index (head f0))
  g1 = g0

decode :: [Hinsi.Hinsi] -> [Ren] -> [Rensetu]
decode _ []      = []
decode hs (x:xs) =
  (MkRensetu i0 h0 t0 f0 g0) : decode hs xs
  where
  (i1,h1,t1,f1,g1) = x
  i0 = i1
  h0 = head (filter ((h1==).Hinsi.index) hs)
  t0 = if t1 == 0
       then []
       else filter ((t1==).KType.index) (Hinsi.ktype h0)
  f0 = if f1 == 0
       then []
       else filter ((f1==).KForm.index) (KType.kform (head t0))
  g0 = g1

compile :: String -> String -> String -> String -> IO ()
compile g f t c = do hs <- Hinsi.readclass g f t
                     cs <- Connect.readconn c
                     let (rs1,rs2) = mktab hs cs
                     let (rs1',rs2') = (encode rs1,encode rs2)
                     writeFile "Ren.hs"
                       ("module Ren  (rensetu_tbl) where\n" ++
                        "import Rensetu\n" ++
                        "rensetu_tbl :: ([Ren],[Ren])\n" ++
                        "rensetu_tbl = " ++ (show (rs1',rs2')) ++ "\n")
                     return ()

indice :: [Rensetu] -> ([String],String,String,String) -> [Int]
indice _ (["文頭"],_,_,_) = []
indice _ (["文末"],_,_,_) = []
indice rs c               = map index (filter (match c) rs)

weave0 :: [[a]] -> [[a]]
weave0 []     = []
weave0 [x]    = map (\t -> [t]) x
weave0 (x:xs) = concat (map (\x' -> map (x':) xs') x)
                where
                xs' = weave0 xs

weave :: [[a]] -> [[a]]
weave = weave0 . (filter (not . null))

convert :: [Connect.Connect] -> [Rensetu] -> [([Int],Int)]
convert []     _  = []
convert (x:xs) rs = (map (\c -> (c,cost)) css) ++ (convert xs rs)
                    where
                    (cs,cost) = x; css = weave (map (indice rs) cs)
