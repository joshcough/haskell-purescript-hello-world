module Home ( Message, def ) where

import Prelude
import Elmish (ComponentDef, ReactComponent, ReactElement, Transition(..), createElement', pureUpdate)
import Data.Functor.Contravariant ((>#<))
import Types (OpM, passToChild)

import Pages (Page(..), PublicPage(..), AuthedPage(..))
import Pages as Pages
import Login as L
import Register as R
import HelloWorld as H

data Message = LoginMessage L.Message | RegisterMessage R.Message | HelloWorldMessage H.Message

type State = { login :: L.State, register :: R.State, hello :: H.State, page :: Page }

emptyState :: State
emptyState = {
    login : L.emptyState
  , register : R.emptyState
  , hello : H.emptyState
  , page: Pages.login
  }

foreign import view_ :: ReactComponent { view :: ReactElement }

def :: ComponentDef OpM Message State
def = { init: emptyState `Transition` [], update, view }
  where
    update s = f where
      f (LoginMessage (L.NavigateTo p)) = navigateTo p
      f (LoginMessage m) = passToLogin s m
      f (RegisterMessage (R.NavigateTo p)) = navigateTo p
      f (RegisterMessage m) = passToRegister s m
      f (HelloWorldMessage (H.NavigateTo p)) = navigateTo p
      f (HelloWorldMessage m) = passToHello s m

      passToLogin    = passToChild L.def.update _.login    _ { login    = _ } LoginMessage
      passToRegister = passToChild R.def.update _.register _ { register = _ } RegisterMessage
      passToHello    = passToChild H.def.update _.hello    _ { hello    = _ } HelloWorldMessage

      navigateTo p@(Public Login)    = pureUpdate $ emptyState { page = p }
      navigateTo p@(Public Register) = pureUpdate $ emptyState { page = p }
      -- todo: here we have to make sure the user is logged in!
      navigateTo p@(Authed p') = pureUpdate $ emptyState { page = p }

      navigateToAuthed p =

    view s dispatch = createElement' view_ {
      view: case s.page of
        Public Login    -> L.def.view s.login    $ dispatch >#< LoginMessage
        Public Register -> R.def.view s.register $ dispatch >#< RegisterMessage
        Authed Hello    -> H.def.view s.hello    $ dispatch >#< HelloWorldMessage
      }
