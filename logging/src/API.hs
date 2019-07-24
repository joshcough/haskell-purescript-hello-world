module API
  ( HelloWorldAPI
  , helloWorldServer
  ) where

import Control.Monad.Except (MonadIO, liftIO)
import Data.Aeson ((.=))
import Models (Message(..))
import Protolude
import Scaffolding.ServantHelpers
import Scaffolding.Logging (logDebug, tShow)
import Scaffolding.Types
import System.Random (randomRIO)

type HelloWorldAPI = "api" :> Compose Api

newtype Api route =
  Api
    { hello :: route :- "hello" :> Get '[JSON] Message
    }
  deriving (Generic)

helloWorldServer :: (MonadIO m) => ServerT HelloWorldAPI (AppT Config m)
helloWorldServer = toServant $ Api {..}
  where
    hello = do
      is <- liftIO $ replicateM 5 (randomRIO (0, 100))
      let m = Message "Hello, World" 42 is
      $(logDebug) "in hello function" ["message" .= tShow m]
      pure m
