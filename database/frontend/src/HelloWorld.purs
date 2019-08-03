module HelloWorld
    ( Message,
      def
    ) where

import Prelude
import Models as Models
import Control.Monad.Error.Class (class MonadError)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Elmish.HTML as R
import Elmish (ReactElement, ComponentDef, DispatchMsg, DispatchMsgFn, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, noData)
import Types (OpM)

data Message = SetCount String | GetMessage | GotMessage Models.Message

type State = { count :: Maybe Int, message :: Maybe Models.Message }

def :: ComponentDef OpM Message State
def =
  { init: { message: Nothing, count: Nothing } `Transition` []
  , update
  , view
  }
  where
    update s (SetCount t) = pureUpdate s { count = fromString t }
    update s GetMessage = s `Transition` [GotMessage <$> getMessage (fromMaybe 5 s.count)]
    update s (GotMessage m) = pureUpdate s { message = Just m }
    view s dispatch = helloWorld s dispatch

localhost :: String
localhost = "http://localhost:8081"

getMessage :: forall m . MonadAff m => MonadError HttpException m => Int -> m Models.Message
getMessage i = httpJSON $ buildReq GET (localhost <> "/api/hello/" <> show i) noData

foreign import view_ :: ReactComponent
  { setCount :: JsCallback (String -> DispatchMsg)
  , getMessage :: JsCallback0
  }

helloWorld :: State -> DispatchMsgFn Message -> ReactElement
helloWorld s dispatch = R.article { className: "container" } $
        [ createElement' view_ {
          setCount: handle dispatch SetCount
        , getMessage: handle dispatch GetMessage
        }
        , R.p {} $ displayMessage s.message
        ]

displayMessage :: Maybe Models.Message -> String
displayMessage (Just (Models.Message r)) =
    r._messageText <> ", " <> show r._messageInt <> ", " <> show r._messageList
displayMessage Nothing = "No Message"