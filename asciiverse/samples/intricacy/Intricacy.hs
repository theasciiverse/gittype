-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE CPP                      #-}
#ifdef APPLE_SDL1
{-# LANGUAGE ForeignFunctionInterface #-}
#endif
module Main where

#ifdef MAIN_SDL1
import qualified SDLUI             (UIM)
import           SDLUIMInstance    ()
#endif

#ifdef MAIN_SDL2
import qualified SDL2UI            (UIM)
import           SDL2UIMInstance   ()
#endif

#ifdef MAIN_CURSES
import qualified CursesUI          (UIM)
import           CursesUIMInstance ()
#endif

import           Init
import           MainState

#ifdef APPLE_SDL1
foreign export ccall hs_MAIN :: IO ()

hs_MAIN :: IO ()
hs_MAIN = main
#endif

main = main'
#ifdef MAIN_SDL1
    (Just (doUI::SDLUI.UIM MainState -> IO (Maybe MainState)))
#else
#ifdef MAIN_SDL2
    (Just (doUI::SDL2UI.UIM MainState -> IO (Maybe MainState)))
#else
    (Nothing :: (Maybe (CursesUI.UIM MainState -> IO (Maybe MainState))))
#endif
#endif
#ifdef MAIN_CURSES
    (Just (doUI::CursesUI.UIM MainState -> IO (Maybe MainState)))
#else
#ifdef MAIN_SDL1
    (Nothing :: (Maybe (SDLUI.UIM MainState -> IO (Maybe MainState))))
#else
#ifdef MAIN_SDL2
    (Nothing :: (Maybe (SDL2UI.UIM MainState -> IO (Maybe MainState))))
#endif
#endif
#endif
