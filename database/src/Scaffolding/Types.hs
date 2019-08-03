module Scaffolding.Types
  ( module Scaffolding.Config
  , AppT
  , App
  , HelloError(..)
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
runAppT app config = (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT app config)
  where
    level = config ^. (loggingCfg . logLevel)
    sourceVersion = config ^. (loggingCfg . logSourceVersion)

-- |
runDb :: (HasConfig c, MonadReader c m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = view configPool >>= liftIO . runSqlPool query

{-
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
    module Config
  , AppT
  , AppT'
  , AppTEnv
  , AppTEnv'
  , App
  , ProverlaysM
  , runAppT
  , runAppToIO
  , runAppTInTest
  , runDb
  ) where

import Protolude

import           Control.Lens                         (view, (^.))
import           Control.Monad                        (when)
import           Control.Monad.Except                 (ExceptT(..), MonadError(..), liftIO, runExceptT)
import           Control.Monad.Reader                 (MonadIO, MonadReader, ReaderT(..))
import           Data.Aeson                           ((.=))
import           Database.Persist.Sql                 (SqlPersistT, runSqlPool)
import           Network.HTTP.Nano                    (HttpError)
import           Web.Rollbar                          (ToRollbarEvent(..), rollbar)

import           Config                               (Config(..), configPool, HasConfig)
import           Error
import           Logging
import           Util.Utils                           (tShow)

import Network.HTTP.Nano.Types (HasHttpCfg)
import Web.Rollbar.Types (HasRollbarCfg)

---
---
---

type ProverlaysM m = (MonadError ChatBotError m, MonadLoggerJSON m)

type AppT m = AppT' ChatBotError m Config
type AppTEnv' e m r = ReaderT r (ExceptT e (LoggingJSONT m))
type AppTEnv m r = AppTEnv' ChatBotError m r
type AppT' e m r = AppTEnv' e m r

type App = AppT IO

-- | Executes the given computation in AppT, logging to stdout with log level configured in the
--   context (via `HasLoggingCfg`), and sending errors to Rollbar (using `HasRollbarCfg`).
runAppT :: forall err r a. (ClassifiedError err, ToRollbarEvent err, Show err, HasHttpCfg r, HasRollbarCfg r, HasLoggingCfg r) => AppT' err IO r a -> r -> IO (Either err a)
runAppT = runAppT' $ \err -> do
    $(logError) "Uncaught app error" ["error" .= tShow err]
    when (isUnexpected err) (rollbar $ toRollbarEvent err)

runAppToIO :: HasLoggingCfg r => r -> AppTEnv IO r a -> IO a
runAppToIO config app' = do
    result <- runAppTInTest app' config
    either (throwIO . fmap (const (ErrorCall "error"))) return result

-- | Runs without rollbar
runAppTInTest :: (HasLoggingCfg r, Show err) => AppT' err IO r a -> r -> IO (Either err a)
runAppTInTest = runAppT' $ \err -> $(logError) "Uncaught app error" ["error" .= tShow err]

-- |
runAppT' :: HasLoggingCfg r =>
       (err -> AppT' HttpError IO r ())
    -> AppT' err IO r a
    -> r
    -> IO (Either err a)
runAppT' onError action context = do
    let handleErrorCallingRollbar :: Either HttpError () -> IO ()
        handleErrorCallingRollbar = either print (const $ return ())
    res <- run level sourceVersion context action
    either (\e -> run level sourceVersion context (onError e) >>= handleErrorCallingRollbar) (const $ pure ()) res
    pure res
    where
        level = context ^. (loggingCfg . logLevel)
        sourceVersion = context ^. (loggingCfg . logSourceVersion)

run :: LogLevel
     -> Maybe SourceVersion
     -> r
     -> ReaderT r (ExceptT e (LoggingJSONT m)) a
     -> m (Either e a)
run level sourceVersion context f =
    (runStdoutLoggingJSONT level sourceVersion . runExceptT) (runReaderT f context)

-}