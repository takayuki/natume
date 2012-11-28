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
module MkDic (
    Lex(MkLex),word,yomi,table,cost,point,readdic
  ) where

import Prelude hiding (id,last)
import System.IO
import Compile (rensetu_tbl1,rensetu_tbl2)
import qualified KForm
import qualified KType
import qualified Rensetu
import ScmParse
import ScmToken

data Lex = MkLex { hinsi :: [String],
                   word  :: String,
                   yomi  :: String,
                   ktype :: String,
                   kform :: String,
                   table :: Int,
                   cost  :: Int,
                   point :: Int  }

type Ren0 = ([String],String,String,String)
type Ren1 = Rensetu.Rensetu
type Ren2 = ([Ren1],[Ren1])
type Ren3 = ([Ren1],[Ren1],[Ren1])

xsearch0 ::  Ren0 -> Ren3 -> Ren2
xsearch0 _   ([],[],zs) = ([],reverse zs)
xsearch0 key ([],ys,zs) = xsearch0 key (ys,[],zs)
xsearch0 key ((x:xs),ys,zs) = if Rensetu.match key x
                              then if null zs
                                   then ((x:xs),ys)
                                   else ((x:xs),ys ++ (reverse zs))
                              else xsearch0 key (xs,ys,x:zs)

xsearch :: Ren0 -> Ren2 -> Ren2
xsearch key (xs,ys) = xsearch0 key (xs,ys,[])

nsearch :: Ren0 -> [Ren1] -> [Ren1]
nsearch key xs = filter (Rensetu.match key) xs

subtail0 :: String -> String -> String -> String
subtail0 s []     ys     = s ++ ys
subtail0 _ _      []     = []
subtail0 s (x:xs) (y:ys) = if x == y then (subtail0 s xs ys) else []

subtail :: String -> String -> String -> String
subtail s x y  = reverse (subtail0 (reverse s) (reverse x) (reverse y))

eval5 :: Ren2 -> Lex -> (Ren2,[Lex])
eval5 r l =
  (r',map (\(tbl,w0,w1,y0,y1,form) ->
              let w = subtail w1 w0 (word l)
                  y = subtail y1 y0 (yomi l) in
                l {word=w,yomi=y,kform=form,table=tbl})
          ss)
  where
  (r',ss) =
    let tbl = Rensetu.index 
        w0 = (KForm.gobi).(KType.basic).head.(Rensetu.ktype)
        w1 = (KForm.gobi).head.(Rensetu.kform)
        y0 = (KForm.ygobi).(KType.basic).head.(Rensetu.ktype)
        y1 = (KForm.ygobi).head.(Rensetu.kform)
        form = (KForm.name).head.(Rensetu.kform)
    in 
      if null (ktype l)
      then 
        let ts = nsearch ((hinsi l),"","",(word l)) rensetu_tbl1
            build x = [(tbl (head x),[],[],[],[],[])] in
          if (not.null) ts
          then (r,build ts)
          else let (r1,r2) = xsearch ((hinsi l),"","","") r in
                 ((r1,r2),build r1)
      else
        let ts = nsearch ((hinsi l),(ktype l),"",(word l)) rensetu_tbl1
            build = map (\t -> (tbl t,w0 t,w1 t,y0 t,y1 t,form t)) in
          if (not.null) ts
          then (r,build ts)
          else let ts' = nsearch ((hinsi l),(ktype l),"","") rensetu_tbl2 in
                 (r,build ts')

eval4 :: [Expr] -> [String]
eval4 []     = []
eval4 (x:xs) = f : eval4 xs where ExprName f = x

eval3 :: Ren2 -> Lex -> [Expr] -> (Ren2,[Lex])
eval3 r l []     = eval5 r l
eval3 r l (x:xs) =
  case x of
    (ExprList [ExprName "見出し語",
               ExprList [ExprName w,ExprInt c]]) ->
      eval3 r (l {word=w,cost=c}) xs
    (ExprList [ExprName "見出し語",
               ExprList [ExprInt w,ExprInt c]]) ->
      eval3 r (l {word=(show w),cost=c}) xs
    (ExprList [ExprName "見出し語",
               ExprList [ExprString w,ExprInt c]]) ->
      eval3 r (l {word=w,cost=c}) xs
    (ExprList [ExprName "見出し語",
               ExprList [ExprName n,ExprAnno (ExprName n'),ExprInt c]]) ->
      (head (map fst ys),concatMap snd ys)
      where
      ys = map (\w -> eval3 r (l {word=w,cost=c}) xs) [n,n++n']
    (ExprList [ExprName "読み",ExprName y]) ->
      eval3 r (l {yomi=y}) xs
    (ExprList [ExprName "読み",ExprString y]) ->
      eval3 r (l {yomi=y}) xs
    (ExprList [ExprName "読み",ExprAlt ns]) ->
      ((fst.head) ys,concatMap snd ys)
      where
      ys = map (\y -> (eval3 r (l {yomi=y}) xs)) (eval4 ns)
    (ExprList [ExprName "読み",ExprAlt ns,ExprName y']) ->
      ((fst.head) ys,concatMap snd ys)
      where
      ys = map (\y -> (eval3 r (l {yomi=y++y'}) xs)) (eval4 ns)
    (ExprList [ExprName "発音",ExprName y]) ->
      (fst z,(snd (eval3 r l xs)) ++ (snd z))
      where
      z = (eval3 r (l {yomi=y}) xs)
    (ExprList [ExprName "発音",ExprString y]) ->
      (fst z,(snd (eval3 r l xs)) ++ (snd z))
      where
      z = (eval3 r (l {yomi=y}) xs)
    (ExprList [ExprName "発音",ExprAlt ns]) ->
      ((fst.head) ys,(snd (eval3 r l xs)) ++ (concatMap snd ys))
      where
      ys = map (\y -> (eval3 r (l {yomi=y}) xs)) (eval4 ns)
    (ExprList [ExprName "発音",ExprAlt ns,ExprName y']) ->
      ((fst.head) ys,(snd (eval3 r l xs)) ++ concatMap snd ys)
      where
      ys = map (\y -> (eval3 r (l {yomi=y++y'}) xs)) (eval4 ns)
    (ExprList [ExprName "活用型",ExprName t]) ->
      eval3 r (l {ktype=t}) xs
    _ ->
      error ("MkDic.eval3: illegal format -- " ++ (show x))

eval2 :: [Expr] -> [String]
eval2 []     = []
eval2 (x:xs) = f : eval2 xs where ExprName f = x

eval1 ::  Ren2 -> (Expr,Expr) -> (Ren2,[Lex])
eval1 r (h,w) =
  case h of
    (ExprList [ExprName "品詞",ExprList name]) ->
      case w of
        ExprList ws -> eval3 r (MkLex (eval2 name) "" "" "" "" 0 0 0) ws
        _ -> error "MkDic.eval1: illegal format"
    _  ->
      error "MkDic.eval1: illegal format"

eval :: Int -> Ren2 -> [Expr] -> [Lex]
eval _ _ []       = []
eval _ _ [_]      = error "MkDic.eval: illegal format"
eval p r (h:w:xs) = map (\l -> l {point=p}) ls ++ (eval (p+1) r' xs)
                    where
                    (r',ls) = eval1 r (h,w)

readdic :: Int -> String -> IO [Lex]
readdic start path =
  do h <- openFile path ReadMode
     hSetEncoding h latin1
     s <- hGetContents h
     return (eval start (rensetu_tbl2,[]) (parse (tokenize s)))
