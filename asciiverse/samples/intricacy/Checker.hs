-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import           Control.Monad.Writer
import           System.Environment
import           System.Exit

import           Frame
import           Hex
import           Lock
import           Mundanities
import           Physics

main = do
    [lockfn,solutionfn] <- getArgs
    Just lock <- readReadFile lockfn
    Just solution <- readReadFile solutionfn

    if checkSolution lock solution then exitSuccess else exitFailure

