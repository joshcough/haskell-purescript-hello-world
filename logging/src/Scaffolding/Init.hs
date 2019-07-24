module Scaffolding.Init where

import Protolude

import Control.Exception (bracket)
import Control.Monad.Except (liftIO, throwError)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Servant.Auth.Server hiding (throwAll)

import API (HelloWorldAPI, helloWorldServer)
import Scaffolding.Config (Config(..), acquireConfig)
import Scaffolding.Types

-- | An action that creates a WAI 'Application' together with its resources,
runApp :: IO ()
runApp = bracket acquireConfig (const $ pure ()) runApp'
  where
    runApp' config = run (_configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize = pure . simpleCors . app

-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg =
  serveWithContext
    (Proxy :: Proxy (HelloWorldAPI :<|> Raw))
    EmptyContext
    (mainServer :<|> serveDirectoryFileServer "frontend")
  where
    convertApp :: Config -> App a -> Handler a
    convertApp cfg' appt = Handler $ liftIO (runAppT appt cfg') >>= either (throwError . const err500) return
    mainServer :: Server HelloWorldAPI
    mainServer =
      hoistServerWithContext
        (Proxy :: Proxy HelloWorldAPI)
        (Proxy :: Proxy '[ CookieSettings, JWTSettings])
        (convertApp cfg)
        helloWorldServer