module PSBridge where

import Language.PureScript.Bridge
import Language.PureScript.Bridge.SumType (order)
import Models (Message)
import Protolude

main :: IO ()
main = writePSTypes "frontend/src" (buildBridge defaultBridge) myTypes

myTypes :: [SumType 'Haskell]
myTypes = [go (Proxy :: Proxy Message)]
  where
    go p = order p (mkSumType p)