module Graphics.Vty.Platform.Unix
  ( mkVty
  , mkVtyWithSettings
  )
where

import Control.Monad (when)

import Graphics.Vty (Vty, installCustomWidthTable, mkVtyFromPair)
import Graphics.Vty.Config (VtyUserConfig(..))

import Graphics.Vty.Platform.Unix.Settings
import Graphics.Vty.Platform.Unix.Output
import Graphics.Vty.Platform.Unix.Input

-- | Create a Vty handle. At most one handle should be created
-- at a time for a given terminal device. Uses the default
-- values for 'UnixSettings'. If you need to override those, use
-- 'mkVtyWithSettings'.
--
-- This may raise
-- 'Graphics.Vty.Platform.Unix.Settings.VtyUnixConfigurationError'.
mkVty :: VtyUserConfig
      -- ^ The user's Vty configuration or the result of
      -- 'defaultConfig'.
      -> IO Vty
mkVty userConfig =
    mkVtyWithSettings userConfig =<< defaultSettings

-- | Create a Vty handle. At most one handle should be created
-- at a time for a given terminal device.
--
-- This also uses the value of @TERM@ to attempt to load and
-- install a Unicode character width table map. For details, see
-- 'Graphics.Vty.UnicodeWidthTable.Install.installUnicodeWidthTable'.
--
-- This may raise
-- 'Graphics.Vty.Platform.Unix.Settings.VtyUnixConfigurationError'.
mkVtyWithSettings :: VtyUserConfig
                  -- ^ The user's Vty configuration or the result of
                  -- 'defaultConfig'.
                  -> UnixSettings
                  -- ^ Runtime settings.
                  -> IO Vty
mkVtyWithSettings userConfig settings = do
    when (configAllowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (configDebugLog userConfig)
                                (Just $ settingTermName settings)
                                (configTermWidthMaps userConfig)

    input <- buildInput userConfig settings
    out <- buildOutput settings
    mkVtyFromPair input out
