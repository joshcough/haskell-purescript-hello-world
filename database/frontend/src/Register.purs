module Register
    ( Message(..),
      State,
      def,
      emptyState
    ) where

import Prelude
import Auth.Models (CreateUser(..), CreateUserResponse(..))
import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Elmish (ComponentDef, DispatchMsg, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData)
import Pages (Page, login)
import Types (OpM)

data Message =
    SetName String
  | SetEmail String
  | SetPassword1 String
  | SetPassword2 String
  | Submit
  | GotResponse CreateUserResponse
  | NavigateTo Page

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
  , login :: JsCallback0
  }

emptyState :: State
emptyState = { name : "", email : "", pw1 : "", pw2 : "", resp : Nothing }

def :: ComponentDef OpM Message State
def =
  { init: emptyState `Transition` []
  , update
  , view
  }
  where
    update s = f where
      f (SetName n) = pureUpdate s { name = n }
      f (SetEmail e) = pureUpdate s { email = e }
      f (SetPassword1 p) = pureUpdate s { pw1 = p }
      f (SetPassword2 p) = pureUpdate s { pw2 = p }
      f Submit = s `Transition` [GotResponse <$> sendCreateReq s]
      f (GotResponse r) = s `Transition` [pure $ NavigateTo login]
      f _ = pureUpdate s

    view s dispatch = createElement' view_ {
        setName: handle dispatch SetName
      , setEmail: handle dispatch SetEmail
      , setPassword1: handle dispatch SetPassword1
      , setPassword2: handle dispatch SetPassword2
      , submit: handle dispatch Submit
      , login: handle dispatch (NavigateTo login)
      }

localhost :: String
localhost = "http://localhost:8081"

sendCreateReq :: forall m . MonadAff m => MonadError HttpException m => State -> m CreateUserResponse
sendCreateReq s = httpJSON $ buildReq POST (localhost <> "/users/register") (jsonData c)
  where
  c = CreateUser { _createUserName: s.name, _createUserEmail: s.email, _createUserPassword: s.pw1}
