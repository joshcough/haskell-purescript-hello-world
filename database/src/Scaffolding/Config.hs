module Scaffolding.Config
  ( Config(..)
  , HasConfig(..)
  , acquireConfig
  ) where

import Control.Lens (makeClassy)
import Control.Monad.Logger (runNoLoggingT)
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS (readFile)
import Data.Text (Text, pack, unpack)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool)
import Database.PostgreSQL.Simple.Internal (postgreSQLConnectionString)
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Prelude (userError)
import Protolude
import Safe (readMay)
import Scaffolding.Logging (HasLoggingCfg(..), LoggingCfg)
import qualified Scaffolding.Logging as Logging
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings)
import System.Environment (getEnv, lookupEnv)

-- | The Config for our application
data Config =
  Config
    { _configPort :: Int
    , _configPool :: ConnectionPool
    , _configCookies :: CookieSettings
    , _configJWT :: JWTSettings
    , _configLogging :: LoggingCfg
    }

makeClassy ''Config

instance HasLoggingCfg Config where
  loggingCfg = configLogging

-- | Allocates resources for 'Config'
acquireConfig :: IO Config
acquireConfig = do
  _configPort <- lookupReadableSetting "PORT" 8081
  _configPool <- acquirePool
  let _configCookies = defaultCookieSettings
  _configJWT <- acquireJWT
  _configLogging <- Logging.fromEnv
  pure Config {..}

-- |
acquirePool :: IO ConnectionPool
acquirePool = makePool =<< lookupRequiredSetting "DATABASE_URL"

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: Text -> IO ConnectionPool
makePool dbUrl =
  case postgreSQLConnectionString <$> parseDatabaseUrl (unpack dbUrl) of
    Nothing -> throwIO (userError "DATABASE_URL malformed.")
    Just url -> runNoLoggingT $ createPostgresqlPool url 1

-- |
acquireJWT :: IO JWTSettings
acquireJWT = defaultJWTSettings <$> readKeyFile "config/jwt.key"
  where
    readKeyFile :: FilePath -> IO JWK
    readKeyFile f = BS.readFile f >>= either (const $ throwIO (userError "DATABASE_URL malformed.")) pure . eitherDecode

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupReadableSetting :: Read a => Text -> a -> IO a
lookupReadableSetting env def = do
  maybeValue <- fmap pack <$> lookupEnv (unpack env)
  maybe (return def) (\str -> maybe (handleFailedRead str) return (readMay $ unpack str)) maybeValue
  where
    handleFailedRead str =
      throwIO . userError . unpack $ mconcat ["Failed to read [[", str, "]] for environment variable ", env]

---- | Looks up a text setting in the environment, with a provided default
--lookupTextSetting :: Text -> Text -> IO Text
--lookupTextSetting env def = maybe def pack <$> lookupEnv (unpack env)

-- |
lookupRequiredSetting :: Text -> IO Text
lookupRequiredSetting e = pack <$> getEnv (unpack e)

---- |
--lookupOptionalSetting :: Text -> IO (Maybe Text)
--lookupOptionalSetting e = fmap pack <$> lookupEnv (unpack e)