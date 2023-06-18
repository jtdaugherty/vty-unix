{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Vty supports a configuration file format and associated 'Config'
-- data type. The 'Config' can be provided to 'mkVty' to customize the
-- application's use of Vty.
--
-- Lines in config files that fail to parse are ignored. Later entries
-- take precedence over earlier ones.
--
-- = Debug
--
-- == @debugLog@
--
-- Format:
--
-- @
--  \"debugLog\" string
-- @
--
-- The value of the environment variable @VTY_DEBUG_LOG@ is equivalent
-- to a debugLog entry at the end of the last config file.
--
-- = Input Processing
--
-- == @map@
--
-- Format:
--
-- @
--  \"map\" term string key modifier_list
--  where
--      key := KEsc | KChar Char | KBS ... (same as 'Key')
--      modifier_list := \"[\" modifier+ \"]\"
--      modifier := MShift | MCtrl | MMeta | MAlt
--      term := "_" | string
-- @
--
-- E.g., if the contents are
--
-- @
--  map _       \"\\ESC[B\"    KUp   []
--  map _       \"\\ESC[1;3B\" KDown [MAlt]
--  map \"xterm\" \"\\ESC[D\"    KLeft []
-- @
--
-- Then the bytes @\"\\ESC[B\"@ will result in the KUp event on all
-- terminals. The bytes @\"\\ESC[1;3B\"@ will result in the event KDown
-- with the MAlt modifier on all terminals. The bytes @\"\\ESC[D\"@ will
-- result in the KLeft event when @TERM@ is @xterm@.
--
-- If a debug log is requested then vty will output the current input
-- table to the log in the above format. A workflow for using this is
-- to set @VTY_DEBUG_LOG@. Run the application. Check the debug log for
-- incorrect mappings. Add corrected mappings to @$HOME\/.vty\/config@.
--
-- = Unicode Character Width Maps
--
-- == @widthMap@
--
-- Format:
--
-- @
--  \"widthMap\" string string
-- @
--
-- E.g.,
--
-- @
--   widthMap \"xterm\" \"\/home\/user\/.vty\/xterm\_map.dat\"
-- @
--
-- This directive specifies the path to a Unicode character width
-- map (the second argument) that should be loaded and used when
-- the value of TERM matches the first argument. Unicode character
-- width maps can be produced either by running the provided binary
-- @vty-build-width-table@ or by calling the library routine
-- 'Graphics.Vty.UnicodeWidthTable.Query.buildUnicodeWidthTable'. The
-- 'Graphics.Vty.mkVty' function will use these configuration settings
-- to attempt to load and install the specified width map. See the
-- documentation for 'Graphics.Vty.mkVty' for details.
module Graphics.Vty.Platform.Unix.Config
  ( Config(..)
  , currentTerminalName
  , standardIOConfig
  )
where

import Control.Applicative hiding (many)
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

-- | A Vty configuration for Unix terminals.
data Config =
    Config { vmin  :: Maybe Int
           -- ^ The default is 1 character.
           , vtime :: Maybe Int
           -- ^ The default is 100 milliseconds, 0.1 seconds.
           , inputFd :: Maybe Fd
           -- ^ The input file descriptor to use. The default is
           -- 'System.Posix.IO.stdInput'
           , outputFd :: Maybe Fd
           -- ^ The output file descriptor to use. The default is
           -- 'System.Posix.IO.stdOutput'
           , termName :: Maybe String
           -- ^ The terminal name used to look up terminfo capabilities.
           -- The default is the value of the TERM environment variable.
           , colorMode :: Maybe ColorMode
           -- ^ The color mode used to know how many colors the terminal
           -- supports.
           }
           deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = mempty

instance Semigroup Config where
    c0 <> c1 =
        -- latter config takes priority for everything but inputMap
        Config { vmin = vmin c1 <|> vmin c0
               , vtime = vtime c1 <|> vtime c0
               , inputFd = inputFd c1 <|> inputFd c0
               , outputFd = outputFd c1 <|> outputFd c0
               , termName = termName c1 <|> termName c0
               , colorMode = colorMode c1 <|> colorMode c0
               }

instance Monoid Config where
    mempty =
        Config { vmin = Nothing
               , vtime = Nothing
               , inputFd = Nothing
               , outputFd = Nothing
               , termName = Nothing
               , colorMode = Nothing
               }
#if !(MIN_VERSION_base(4,11,0))
    mappend = (<>)
#endif

standardIOConfig :: IO Config
standardIOConfig = do
    mb <- lookupEnv termVariable
    case mb of
      Nothing -> throwIO VtyMissingTermEnvVar
      Just t -> do
        mcolorMode <- detectColorMode t
        return defaultConfig
          { vmin               = Just 1
          , vtime              = Just 100
          , inputFd            = Just stdInput
          , outputFd           = Just stdOutput
          , termName           = Just t
          , colorMode          = Just mcolorMode
          }

termVariable :: String
termVariable = "TERM"

currentTerminalName :: IO (Maybe String)
currentTerminalName = lookupEnv termVariable
