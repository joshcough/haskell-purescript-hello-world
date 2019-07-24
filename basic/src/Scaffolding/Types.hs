module Scaffolding.Types
  ( module Scaffolding.Config
  , AppT
  , App
  , HelloError(..)
  , runAppT
  ) where

import Protolude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Scaffolding.Config (Config(..))

newtype HelloError =
  HelloError
    { _helloErrorMessage :: Text
    }
  deriving  (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

type AppT m = ReaderT Config (ExceptT HelloError m)

type App = AppT IO

runAppT :: AppT IO a -> Config -> IO (Either HelloError a)
runAppT app config = runExceptT (runReaderT app config)