module Scaffolding.Config
  ( Config(..)
  , acquireConfig
  ) where

import Prelude (error)
import Protolude

import Control.Lens (makeClassy)
import Data.Text (Text, pack, unpack)
import Safe (readMay)
import Scaffolding.Logging (HasLoggingCfg(..), LoggingCfg)
import qualified Scaffolding.Logging as Logging

import System.Environment (lookupEnv)

-- | The Config for our application
data Config =
  Config
    { _configPort :: Int
    , _configLogging :: LoggingCfg
    }
  deriving  (Eq, Ord, Show, Generic)

makeClassy ''Config

instance HasLoggingCfg Config where
    loggingCfg = configLogging

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = Config <$> lookupReadableSetting "PORT" 8081 <*> Logging.fromEnv

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupReadableSetting :: Read a => Text -> a -> IO a
lookupReadableSetting env def = do
  maybeValue <- fmap pack <$> lookupEnv (unpack env)
  maybe (return def) (\str -> maybe (handleFailedRead str) return (readMay $ unpack str)) maybeValue
  where
    handleFailedRead str = error . unpack $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]