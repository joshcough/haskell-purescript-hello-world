module HelloWorld
    ( Message,
      def
    ) where

import Prelude
import Models as Models
import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Elmish.HTML as R
import Elmish (ReactElement, ComponentDef, DispatchMsgFn, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

data Message = GetMessage | GotMessage Models.Message

type State = { message :: Maybe Models.Message }

def :: ComponentDef OpM Message State
def =
  { init: Transition { message: Nothing } []
  , update
  , view
  }
  where
    update s GetMessage = Transition s [ GotMessage <$> getMessage ]
    update s (GotMessage m) = pureUpdate s { message = Just m }
    view s dispatch = helloWorld s dispatch

getMessage :: forall m . MonadAff m => MonadError HttpException m => m Models.Message
getMessage = httpJSON $ buildReq GET "http://localhost:8081/api/hello" noData

foreign import view_ :: ReactComponent
  { getMessage :: JsCallback0
  }

helloWorld :: State -> DispatchMsgFn Message -> ReactElement
helloWorld s dispatch = R.article { className: "container" } $
        [ createElement' view_ { getMessage: handle dispatch GetMessage }
        , R.p {} $ displayMessage s.message
        ]

displayMessage :: Maybe Models.Message -> String
displayMessage (Just (Models.Message r)) =
    r._messageText <> ", " <> show r._messageInt <> ", " <> show r._messageList
displayMessage Nothing = "No Message"