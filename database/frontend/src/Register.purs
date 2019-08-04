module Register
    ( Message,
      def
    ) where

import Prelude
import Auth.Models (CreateUser(..), CreateUserResponse(..))
import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsg, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData)
import Types (OpM)
import Debug.Trace (spy)

data Message =
    SetName String
  | SetEmail String
  | SetPassword1 String
  | SetPassword2 String
  | Submit
  | GotResponse CreateUserResponse

type State = {
    name :: String
  , email :: String
  , pw1 :: String
  , pw2 :: String
  , resp :: Maybe CreateUserResponse
  }

foreign import view_ :: ReactComponent
  { setName :: JsCallback (String -> DispatchMsg)
  , setEmail :: JsCallback (String -> DispatchMsg)
  , setPassword1 :: JsCallback (String -> DispatchMsg)
  , setPassword2 :: JsCallback (String -> DispatchMsg)
  , submit :: JsCallback0
  }

def :: ComponentDef OpM Message State
def =
  { init: emptyState `Transition` []
  , update
  , view
  }
  where
    emptyState = { name : "", email : "", pw1 : "", pw2 : "", resp : Nothing }

    update s = f where
      f (SetName n) = pureUpdate s { name = n }
      f (SetEmail e) = pureUpdate s { email = e }
      f (SetPassword1 p) = pureUpdate s { pw1 = p }
      f (SetPassword2 p) = pureUpdate s { pw2 = p }
      f Submit = s `Transition` [GotResponse <$> sendCreateReq s]
      f (GotResponse r) = pureUpdate s { resp = Just (spy "response" r) }

    view s dispatch = createElement' view_ {
        setName: handle dispatch SetName
      , setEmail: handle dispatch SetEmail
      , setPassword1: handle dispatch SetPassword1
      , setPassword2: handle dispatch SetPassword2
      , submit: handle dispatch Submit
      }

localhost :: String
localhost = "http://localhost:8081"

sendCreateReq :: forall m . MonadAff m => MonadError HttpException m => State -> m CreateUserResponse
sendCreateReq s = httpJSON $ buildReq POST (localhost <> "/users/register") (jsonData c)
  where
  c = CreateUser { _createUserName: s.name, _createUserEmail: s.email, _createUserPassword: s.pw1}
