
module Auth.PSBridge where

import Protolude
import Data.Proxy
import Language.PureScript.Bridge
import Language.PureScript.Bridge.SumType (equal)
import Auth.Models

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes

-- TODO: there are more types in Models that maybe should be added here
myTypes :: [SumType 'Haskell]
myTypes = [
    let p = (Proxy :: Proxy User)                in equal p (mkSumType p)
  , let p = (Proxy :: Proxy CreateUser)          in equal p (mkSumType p)
  ]
