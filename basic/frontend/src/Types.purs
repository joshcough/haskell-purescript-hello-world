module Types
    ( OpM, OpM', runOpM, runOpM'
    ) where

import Prelude
import Models as Models
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Array (length)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff (Aff, error)
import Elmish.HTML as R
import Elmish (ReactElement, boot, ComponentDef, nat, DispatchMsgFn, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)

type OpM' c = ReaderT c (ExceptT HttpException Aff)
type OpM = OpM' Unit

runOpM :: forall a . OpM a -> Aff a
runOpM = runOpM' unit

runOpM' :: forall context a . context -> OpM' context a -> Aff a
runOpM' context f = do
  res <- runExceptT (runReaderT f context)
  either (throwError <<< error <<< show) pure res
