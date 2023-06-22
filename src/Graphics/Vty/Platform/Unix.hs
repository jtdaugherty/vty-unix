module Graphics.Vty.Platform.Unix
  ( mkVty
  )
where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Graphics.Vty (Vty, installCustomWidthTable, mkVtyFromPair)
import Graphics.Vty.Config (VtyUserConfig(..))

import Graphics.Vty.Platform.Unix.Settings
import Graphics.Vty.Platform.Unix.Output
import Graphics.Vty.Platform.Unix.Input

-- | Create a Vty handle. At most one handle should be created at a time
-- for a given terminal device.
--
-- For most applications, @mkVty defaultConfig Nothing@ is sufficient.
--
-- This may raise 'VtyConfigurationError'.
mkVty :: VtyUserConfig
      -- ^ The user's Vty configuration or the result of
      -- 'defaultConfig'.
      -> Maybe UnixSettings
      -- ^ Runtime settings to override defaults; see 'defaultSettings'
      -- for defaults.
      -> IO Vty
mkVty userConfig mUnixConfig = do
    settings <- fromMaybe <$> defaultSettings <*> pure mUnixConfig

    when (configAllowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (configDebugLog userConfig)
                                (Just $ settingTermName settings)
                                (configTermWidthMaps userConfig)

    input <- buildInput userConfig settings
    out <- buildOutput settings
    mkVtyFromPair input out
