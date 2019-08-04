module Pages
    ( Page(..),
      PublicPage(..),
      AuthedPage(..),
      login,
      register,
      hello
    ) where

data Page =
    Public PublicPage
  | Authed AuthedPage

data PublicPage =
    Login
  | Register

data AuthedPage =
    Hello

login :: Page
login = Public Login
register :: Page
register = Public Register
hello :: Page
hello = Authed Hello