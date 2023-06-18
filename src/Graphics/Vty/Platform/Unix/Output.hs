{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides functions for accessing the current terminal
-- or a specific terminal device.
--
-- See also:
--
-- 1. "Graphics.Vty.Output": This instantiates an abtract interface
-- to the terminal based on the @TERM@ and @COLORTERM@ environment
-- variables.
--
-- 2. "Graphics.Vty.Output.Interface": Defines the generic interface all
-- terminal modules need to implement.
--
-- 3. "Graphics.Vty.Output.TerminfoBased": Defines a terminal instance
-- that uses terminfo for all control strings. No attempt is made to
-- change the character set to UTF-8 for these terminals.
--
-- 4. "Graphics.Vty.Output.XTermColor": This module contains an
-- interface suitable for xterm-like terminals. These are the terminals
-- where @TERM@ begins with @xterm@. This does use terminfo for as many
-- control codes as possible.
module Graphics.Vty.Platform.Unix.Output
  ( outputForConfig
  )
where

import Graphics.Vty.Output.Interface

import Graphics.Vty.Platform.Unix.Config
import Graphics.Vty.Platform.Unix.Output.XTermColor as XTermColor
import Graphics.Vty.Platform.Unix.Output.TerminfoBased as TerminfoBased

import Data.List (isPrefixOf)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Returns an `Output` for the terminal specified in `Config`.
--
-- The specific Output implementation used is hidden from the API user.
-- All terminal implementations are assumed to perform more, or less,
-- the same. Currently, all implementations use terminfo for at least
-- some terminal specific information.
--
-- If a terminal implementation is developed for a terminal without
-- terminfo support then Vty should work as expected on that terminal.
--
-- Selection of a terminal is done as follows:
--
--      * If TERM starts with "xterm", "screen" or "tmux", use XTermColor.
--      * otherwise use the TerminfoBased driver.
outputForConfig :: Config -> IO Output
outputForConfig Config{ outputFd = fd, termName = termName
                      , colorMode = colorMode } = do
    t <- if isXtermLike termName
         then XTermColor.reserveTerminal termName fd colorMode
         -- Not an xterm-like terminal. try for generic terminfo.
         else TerminfoBased.reserveTerminal termName fd colorMode

    return t

isXtermLike :: String -> Bool
isXtermLike termName =
    any (`isPrefixOf` termName) xtermLikeTerminalNamePrefixes

xtermLikeTerminalNamePrefixes :: [String]
xtermLikeTerminalNamePrefixes =
    [ "xterm"
    , "screen"
    , "tmux"
    , "rxvt"
    ]
