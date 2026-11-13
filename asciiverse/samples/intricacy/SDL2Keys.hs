-- This file is part of Intricacy
-- Copyright (C) 2013-2025 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SDL2Keys (keysymChar) where

import           Data.Bits          (xor)
import           Data.Char          (chr, ord, toUpper)
import           SDL.Input.Keyboard

import           Util               (fi)

applyCtrl, applyMeta :: Char -> Char
applyCtrl ch | c <- fromEnum $ toUpper ch, fromEnum 'A' <= c, c <= fromEnum 'Z' = toEnum $ xor 64 c
applyCtrl ch = ch
applyMeta = toEnum . xor 128 . fromEnum

keysymChar :: Keysym -> Char
keysymChar (Keysym _ k mods) =
    let shift = keyModifierLeftShift mods || keyModifierRightShift mods
        ctrl = keyModifierLeftCtrl mods || keyModifierRightCtrl mods
        meta = keyModifierLeftAlt mods || keyModifierRightAlt mods
    in (if meta then applyMeta else id) .
        (if ctrl then applyCtrl . toUpper else id) $
        case k of
            Keycode n | unwrapKeycode KeycodeA <= n && n <= unwrapKeycode KeycodeZ ->
                chr (ord (if shift then 'A' else 'a') + fi (n - unwrapKeycode KeycodeA))
            Keycode n | unwrapKeycode KeycodeKP1 <= n && n <= unwrapKeycode KeycodeKP9 ->
                chr (ord '1' + fi (n - unwrapKeycode KeycodeKP1))
            KeycodeReturn     -> '\r'
            KeycodeEscape     -> '\ESC'
            KeycodeBackspace  -> '\b'
            KeycodeTab        -> '\t'
            KeycodeSpace      -> ' '
            KeycodeExclaim -> '!'
            KeycodeQuoteDbl -> '"'
            KeycodeHash -> '#'
            KeycodePercent -> '%'
            KeycodeDollar -> '$'
            KeycodeAmpersand -> '&'
            KeycodeQuote -> if shift then '"' else '\''
            KeycodeLeftParen -> '('
            KeycodeRightParen -> ')'
            KeycodeAsterisk -> '*'
            KeycodePlus -> '+'
            KeycodeComma -> if shift then '<' else ','
            KeycodeMinus -> if shift then '_' else '-'
            KeycodePeriod -> if shift then '>' else '.'
            KeycodeSlash -> if shift then '?' else '/'
            Keycode1 -> if shift then '!' else '1'
            Keycode2 -> if shift then '@' else '2'
            Keycode3 -> if shift then '#' else '3'
            Keycode4 -> if shift then '$' else '4'
            Keycode5 -> if shift then '%' else '5'
            Keycode6 -> if shift then '^' else '6'
            Keycode7 -> if shift then '&' else '7'
            Keycode8 -> if shift then '*' else '8'
            Keycode9 -> if shift then '(' else '9'
            Keycode0 -> if shift then ')' else '0'
            KeycodeColon -> ':'
            KeycodeSemicolon -> if shift then ':' else ';'
            KeycodeLess -> '<'
            KeycodeEquals -> if shift then '+' else '='
            KeycodeGreater -> '>'
            KeycodeQuestion -> '?'
            KeycodeAt -> '@'
            KeycodeLeftBracket -> if shift then '{' else '['
            KeycodeBackslash -> if shift then '|' else '\\'
            KeycodeRightBracket -> if shift then '}' else ']'
            KeycodeCaret -> '^'
            KeycodeUnderscore -> '_'
            KeycodeBackquote -> if shift then '~' else '`'
            KeycodeRight      -> '→'
            KeycodeLeft       -> '←'
            KeycodeDown       -> '↓'
            KeycodeUp         -> '↑'
            KeycodeKPDivide   -> if shift then '?' else '/'
            KeycodeKPMultiply -> '*'
            KeycodeKPMinus    -> '-'
            KeycodeKPPlus     -> '+'
            KeycodeKPEnter    -> '\n'
            KeycodeKP0 -> '0'
            KeycodeKPPeriod -> '.'
            _ -> '\0'
