-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Util where
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- nice tip from one joeyh:
whenM,unlessM,(>>?),(>>!) :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= flip when a
unlessM c a = c >>= flip unless a
(>>?) = whenM
(>>!) = unlessM
-- same precedence as ($), allowing e.g. foo bar >>! error $ "failed " ++ meep
infixr 0 >>?
infixr 0 >>!

-- |fi: just a straight abbreviation for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
