module Scaffolding.Types
  ( module Scaffolding.Config
  , AppT
  , App
  , HelloError(..)
  , runAppT
  ) where

import Protolude

import Control.Lens ((^.))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Scaffolding.Config (Config(..))
import Scaffolding.Logging

newtype HelloError =
  HelloError
    { _helloErrorMessage :: Text
    }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type AppT r m = ReaderT r (ExceptT HelloError (LoggingJSONT m))

type App = AppT Config IO

runAppT :: HasLoggingCfg r => AppT r IO a -> r -> IO (Either HelloError a)
runAppT app config = (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT app config)
  where
    level = config ^. (loggingCfg . logLevel)
    sourceVersion = config ^. (loggingCfg . logSourceVersion)
