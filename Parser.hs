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
module Parser (
    Parser(MkParser),
    applyParser,
    zero,one,plus,orelse,many,some
  ) where

data Parser a b = MkParser ([a] -> [(b,[a])])

applyParser :: Parser a b -> ([a] -> [(b,[a])])
applyParser (MkParser f) = f

instance Monad (Parser a) where
  return x = MkParser f
             where f s = [(x,s)]

  p >>= q = MkParser f
            where f s = [(t2,r2) | (t1,r1) <- applyParser p s,
                                   (t2,r2) <- applyParser (q t1) r1]

zero :: Parser a b
zero = MkParser f where f _ = []

one :: Parser a [b]
one = MkParser f where f s = [([],s)]

plus :: Parser a b -> Parser a b -> Parser a b
plus p q = MkParser f
           where f s = applyParser p s ++ applyParser q s

orelse :: Parser a b -> Parser a b -> Parser a b
orelse p q  = MkParser f
              where f s = let ps = applyParser p s in
                          if not (null ps) then ps else applyParser q s

some :: Parser a b -> Parser a [b]
some f = do x <- f; xs <- many f; return (x:xs)

many :: Parser a b -> Parser a [b]
many f = some f `plus` one
