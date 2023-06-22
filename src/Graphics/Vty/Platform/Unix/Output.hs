{-# LANGUAGE RecordWildCards, CPP #-}
-- | This module provides a function to build an 'Output' for Unix
-- terminal devices.
module Graphics.Vty.Platform.Unix.Output
  ( buildOutput
  )
where

import Graphics.Vty.Output

import Graphics.Vty.Platform.Unix.Settings
import Graphics.Vty.Platform.Unix.Output.XTermColor as XTermColor
import Graphics.Vty.Platform.Unix.Output.TerminfoBased as TerminfoBased

import Data.List (isPrefixOf)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Returns an `Output` for the terminal specified in `UnixSettings`.
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
buildOutput :: UnixSettings -> IO Output
buildOutput settings = do
    let termName = settingTermName settings
        fd = settingOutputFd settings
        colorMode = settingColorMode settings

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
