module Auth.PSBridge where

import Protolude
import Language.PureScript.Bridge
import Language.PureScript.Bridge.SumType (order)
import Auth.Models

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes

myTypes :: [SumType 'Haskell]
myTypes = [
    go (Proxy :: Proxy CreateUser)
  , go (Proxy :: Proxy CreateUserResponse)
  , go (Proxy :: Proxy Login)
  , go (Proxy :: Proxy User)
  ]
  where go p = order p (mkSumType p)
