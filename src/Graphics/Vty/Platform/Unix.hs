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
-- The specified configuration is added to the the configuration
-- loaded by 'userConfig' with the 'userConfig' configuration taking
-- precedence. See "Graphics.Vty.Config".
--
-- For most applications @mkVty defaultConfig Nothing@ is sufficient.
mkVty :: VtyUserConfig -> Maybe UnixSettings -> IO Vty
mkVty userConfig mUnixConfig = do
    settings <- fromMaybe <$> defaultSettings <*> pure mUnixConfig

    when (configAllowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (configDebugLog userConfig)
                                (Just $ settingTermName settings)
                                (configTermWidthMaps userConfig)

    input <- buildInput userConfig settings
    out <- buildOutput settings
    mkVtyFromPair input out
