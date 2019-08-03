module API
  ( HelloWorldAPI
  , helloWorldServer
  ) where

import Control.Monad.Except (MonadIO, liftIO)
import Models (Message(..))
import Protolude
import Scaffolding.ServantHelpers
import Scaffolding.Types
import System.Random (randomRIO)

type HelloWorldAPI = "api" :> Compose Api

newtype Api route =
  Api
    { hello :: route :- "hello" :> Capture "count" Int :> Get '[JSON] Message
    }
  deriving (Generic)

helloWorldServer :: (MonadIO m) => ServerT HelloWorldAPI (AppT m)
helloWorldServer = toServant $ Api {..}
  where
    hello i = do
      is <- liftIO $ replicateM i (randomRIO (0, 100))
      pure $ Message "Hello, World" i is