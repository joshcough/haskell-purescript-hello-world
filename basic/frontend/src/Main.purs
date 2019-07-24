module Main where

import Prelude

import Elmish (boot, nat)
import Effect (Effect)
import HelloWorld (def)
import Types (runOpM)

main :: Effect Unit
main = boot { domElementId: "app" , def: nat runOpM def }
