module Login
    ( Message,
      def
    ) where

import Prelude
import Auth.Models (Login(..))
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsg, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData)
import Types (OpM)

data Message =
    SetEmail String
  | SetPassword String
  | Submit
  | GotResponse

type State = {
    email :: String
  , pw :: String
  }

foreign import view_ :: ReactComponent
  { setEmail :: JsCallback (String -> DispatchMsg)
  , setPassword :: JsCallback (String -> DispatchMsg)
  , submit :: JsCallback0
  }

def :: ComponentDef OpM Message State
def =
  { init: emptyState `Transition` []
  , update
  , view
  }
  where
    emptyState = { email : "", pw : "" }

    update s = f where
      f (SetEmail e) = pureUpdate s { email = e }
      f (SetPassword p) = pureUpdate s { pw = p }
      f Submit = s `Transition` [pure GotResponse <* sendLoginReq s]
      f GotResponse = pureUpdate s

    view s dispatch = createElement' view_ {
        setEmail: handle dispatch SetEmail
      , setPassword: handle dispatch SetPassword
      , submit: handle dispatch Submit
      }

localhost :: String
localhost = "http://localhost:8081"

sendLoginReq :: forall m . MonadAff m => MonadError HttpException m => State -> m Unit
sendLoginReq s = httpJSON $ buildReq POST (localhost <> "/users/login") (jsonData c)
  where
  c = Login { _loginEmail: s.email, _loginPassword: s.pw }
