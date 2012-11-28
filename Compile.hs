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
module Compile (
    hinsi,rensetu_tbl1,rensetu_tbl2
  ) where

import Ctypes
import Cforms
import Grammar
import Ren
import qualified Hinsi
import qualified Katuyou
import qualified Rensetu

hinsi :: [Hinsi.Hinsi]
hinsi = Hinsi.enum (Hinsi.merge grammar (Katuyou.typeform ctypes cforms))

rensetu_tbl1,rensetu_tbl2 :: [Rensetu.Rensetu]
(rensetu_tbl1,rensetu_tbl2) =
   (Rensetu.decode hinsi rs1,Rensetu.decode hinsi rs2)
   where
   (rs1,rs2) = Ren.rensetu_tbl
