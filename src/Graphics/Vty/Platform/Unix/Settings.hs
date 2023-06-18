{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Vty.Platform.Unix.Settings
  ( UnixSettings(..)
  , currentTerminalName
  , defaultSettings
  )
where

import Control.Exception (Exception(..), throwIO)
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Typeable (Typeable)
import System.Posix.IO (stdInput, stdOutput)
import System.Environment (lookupEnv)
import System.Posix.Types (Fd(..))

import Graphics.Vty.Attributes.Color
import Graphics.Vty.Platform.Unix.Output.Color (detectColorMode)

-- | Type of errors that can be thrown when configuring VTY
data VtyConfigurationError =
    VtyMissingTermEnvVar
    -- ^ TERM environment variable not set
    deriving (Show, Eq, Typeable)

instance Exception VtyConfigurationError where
    displayException VtyMissingTermEnvVar = "TERM environment variable not set"

-- | Runtime library settings for interacting with Unix terminals.
data UnixSettings =
    UnixSettings { settingVmin :: Int
                 , settingVtime :: Int
                 , settingInputFd :: Fd
                 -- ^ The input file descriptor to use.
                 , settingOutputFd :: Fd
                 -- ^ The output file descriptor to use.
                 , settingTermName :: String
                 -- ^ The terminal name used to look up terminfo capabilities.
                 , settingColorMode :: ColorMode
                 -- ^ The color mode used to know how many colors the terminal
                 -- supports.
                 }
                 deriving (Show, Eq)

defaultSettings :: IO UnixSettings
defaultSettings = do
    mb <- lookupEnv termVariable
    case mb of
      Nothing -> throwIO VtyMissingTermEnvVar
      Just t -> do
        mcolorMode <- detectColorMode t
        return $ UnixSettings { settingVmin      = 1
                              , settingVtime     = 100
                              , settingInputFd   = stdInput
                              , settingOutputFd  = stdOutput
                              , settingTermName  = t
                              , settingColorMode = mcolorMode
                              }

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable
