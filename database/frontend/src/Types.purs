module Types
    ( OpM, OpM', runOpM, runOpM', passToChild
    ) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Data.Either (either)
import Effect.Aff (Aff, error)
import Network.HTTP (HttpException)

import Elmish ((<$$>), ReactElement, ComponentDef, DispatchMsg, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)


type OpM' c = ReaderT c (ExceptT HttpException Aff)
type OpM = OpM' Unit

runOpM :: forall a . OpM a -> Aff a
runOpM = runOpM' unit

runOpM' :: forall context a . context -> OpM' context a -> Aff a
runOpM' context f = do
  res <- runExceptT (runReaderT f context)
  either (throwError <<< error <<< show) pure res

passToChild
    :: forall parentState parentMsg childMsg childState
     . (childState -> childMsg -> Transition OpM childMsg childState) -- child's update
    -> (parentState -> childState)                                    -- extract child childState
    -> (parentState -> childState -> parentState)                     -- embed child childState
    -> (childMsg -> parentMsg)                                        -- wrap child message
    -> parentState
    -> childMsg
    -> Transition OpM parentMsg parentState
passToChild updateFunction lowerState embedState liftMessage st childMsg =
    let Transition newChildState fx = updateFunction (lowerState st) childMsg
    in Transition (embedState st newChildState) (liftMessage <$$> fx)
