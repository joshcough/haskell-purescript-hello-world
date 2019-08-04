module Scaffolding.Types
  ( module Scaffolding.Config
  , AppT
  , App
  , HelloError
  , runAppT
  , runDb
  ) where

import Protolude

import Control.Lens ((^.), view)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..))
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Scaffolding.Config (Config(..), HasConfig(..))
import Scaffolding.Error (HelloError)
import Scaffolding.Logging

type AppT r m = ReaderT r (ExceptT HelloError (LoggingJSONT m))

type App = AppT Config IO

runAppT :: HasLoggingCfg r => AppT r IO a -> r -> IO (Either HelloError a)
runAppT app conf = (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT app conf)
  where
    level = conf ^. (loggingCfg . logLevel)
    sourceVersion = conf ^. (loggingCfg . logSourceVersion)

-- |
runDb :: (HasConfig c, MonadReader c m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = view configPool >>= liftIO . runSqlPool query
