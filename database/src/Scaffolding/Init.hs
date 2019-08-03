module Scaffolding.Init where

import Protolude

import API (app)
import Control.Exception (bracket)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Scaffolding.Config (Config(..), acquireConfig)

-- | An action that creates a WAI 'Application' together with its resources,
runApp :: IO ()
runApp = bracket acquireConfig (const $ pure ()) runApp'
  where
    runApp' config = run (_configPort config) =<< initialize config

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: Config -> IO Application
initialize = pure . simpleCors . app

