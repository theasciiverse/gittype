-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module CVec where

import           Data.Monoid
import           Data.Semigroup as Sem
import           Hex

data CVec = CVec { cy, cx :: Int }
    deriving (Eq, Ord, Show)
instance Sem.Semigroup CVec where
    (CVec y x) <> (CVec y' x') = CVec (y+y') (x+x')
instance Monoid CVec where
    mempty = CVec 0 0
    mappend = (Sem.<>)
instance Grp CVec where
    neg (CVec y x) = CVec (-y) (-x)
type CCoord = PHS CVec

hexVec2CVec :: HexVec -> CVec
hexVec2CVec (HexVec x y z) = CVec (-y) (x-z)
cVec2HexVec :: CVec -> HexVec
cVec2HexVec (CVec y x) = HexVec ((x+y)`div`2) (-y) ((y-x)`div`2)

truncateCVec :: CVec -> CVec
truncateCVec (CVec x y) = CVec (max 0 x) (max 0 y)
