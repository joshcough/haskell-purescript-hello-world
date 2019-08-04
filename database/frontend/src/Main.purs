module Main where

import Prelude

import Elmish (boot, nat)
import Effect (Effect)
import Home (def)
import Types (runOpM)

{-
Here is what we need:

When a user comes to index.html

If they are logged in, we should show them the HelloWorld page.

If not, we should show them the login page.

The login page should have only two fields:
   email
   pw

On the login page there should also be a link to the register page.

The register page should have
  Name
  Email
  PW
  PW again

When a user completes the register form, it should send them to the login page.

Questions:

   How do we tell if a user is logged in?
   Can we get this from a cookie?
   Do we need to use local storage for anything, or just cookies?

   answer: we should probably take it from the cookie, and put it in local storage.
   when they logout, we should remove it from the cookie and from local storage
-}

main :: Effect Unit
main = boot { domElementId: "app" , def: nat runOpM def }
