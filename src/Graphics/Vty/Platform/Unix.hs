module Graphics.Vty.Platform.Unix
  ( mkVty
  )
where

import Control.Monad (when)

import Graphics.Vty (Vty, installCustomWidthTable, mkVtyFromPair)
import Graphics.Vty.Config (VtyUserConfig(..))

import Graphics.Vty.Platform.Unix.Config
import Graphics.Vty.Platform.Unix.Output
import Graphics.Vty.Platform.Unix.Input

-- | Create a Vty handle. At most one handle should be created at a time
-- for a given terminal device.
--
-- The specified configuration is added to the the configuration
-- loaded by 'userConfig' with the 'userConfig' configuration taking
-- precedence. See "Graphics.Vty.Config".
--
-- For most applications @mkVty defaultConfig@ is sufficient.
mkVty :: VtyUserConfig -> Config -> IO Vty
mkVty userConfig appConfig = do
    when (allowCustomUnicodeWidthTables userConfig /= Just False) $
        installCustomWidthTable (debugLog userConfig)
                                (termName appConfig)
                                (termWidthMaps userConfig)

    input <- inputForConfig userConfig appConfig
    out <- outputForConfig appConfig
    mkVtyFromPair input out
