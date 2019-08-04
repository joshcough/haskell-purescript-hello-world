module Home
    ( Message,
      def
    ) where

import Prelude
import Control.Monad.Error.Class (class MonadError)
import Effect.Aff.Class (class MonadAff)
import Elmish ((<$$>), ReactElement, ComponentDef, DispatchMsg, JsCallback, JsCallback0, ReactComponent, Transition(..), createElement', handle, pureUpdate)
import Network.HTTP (HttpException, Method(..), buildReq, httpJSON, jsonData)
import Pages (Page(..), PublicPage(..), AuthedPage(..), hello)
import Pages as Pages
import Types (OpM)
import Data.Maybe (Maybe(..))
import Data.Functor.Contravariant ((>#<))

import Login as Login
import Register as Register
import HelloWorld as HelloWorld

data Message =
    LoginMessage Login.Message
  | RegisterMessage Register.Message
  | HelloWorldMessage HelloWorld.Message

type State = {
    loginState :: Login.State
  , registerState :: Register.State
  , helloState :: HelloWorld.State
  , page :: Page
  }

emptyState :: State
emptyState = {
      loginState : Login.emptyState
    , registerState : Register.emptyState
    , helloState : HelloWorld.emptyState
    , page: Pages.login }


foreign import view_ :: ReactComponent
  { view :: ReactElement
  }

def :: ComponentDef OpM Message State
def =
  { init: emptyState `Transition` []
  , update
  , view
  }
  where
    update s = f where
      f (LoginMessage (Login.NavigateTo p)) = pureUpdate $ navigateTo s p
      f (LoginMessage m) = Transition s {loginState = s'} (LoginMessage <$$> fx)
        where Transition s' fx = Login.def.update s.loginState m

      f (RegisterMessage (Register.NavigateTo p)) = pureUpdate $ navigateTo s p
      f (RegisterMessage m) = Transition s {registerState = s'} (RegisterMessage <$$> fx)
        where Transition s' fx = Register.def.update s.registerState m

      f (HelloWorldMessage (HelloWorld.NavigateTo p)) = pureUpdate $ navigateTo s p
      f (HelloWorldMessage m) = Transition s {helloState = s'} (HelloWorldMessage <$$> fx)
        where Transition s' fx = HelloWorld.def.update s.helloState m

      navigateTo :: State -> Page -> State
      navigateTo s p@(Public Login) = emptyState { page = p }
      navigateTo s p@(Public Register) = emptyState { page = p }
      -- todo: here we have to make sure the user is logged in!
      navigateTo s p@(Authed Hello) = emptyState { page = p }

    view s dispatch = createElement' view_ {
      view: case s.page of
        Public Login -> Login.def.view s.loginState $ dispatch >#< LoginMessage
        Public Register -> Register.def.view s.registerState $ dispatch >#< RegisterMessage
        Authed Hello -> HelloWorld.def.view s.helloState $ dispatch >#< HelloWorldMessage
      }

