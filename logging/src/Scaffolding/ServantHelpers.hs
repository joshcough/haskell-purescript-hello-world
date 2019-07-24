module Scaffolding.ServantHelpers
  ( module Scaffolding.Config
  , module Servant
  , module Servant.API.Generic
  , module Servant.Server.Generic
  , Compose
  , toServant
  ) where

import Protolude

import Control.Monad.Except (throwError)
import Scaffolding.Config (Config(..))

import Servant
import Servant.API.Generic hiding (toServant)
import qualified Servant.API.Generic as S
import Servant.Server.Generic (AsServerT, genericServerT)

---
--- servant generic helpers
---
-- This alias encapsulates this frequently repeated pattern that is used for
-- nesting Servant.API.Generic-powered APIs.
type Compose api = S.ToServant api S.AsApi

-- This function used to be necessary before servant-generic was merged into the
-- main servant package, it provided some better type inference for use sites.
-- Currently it's no longer necessary - both its type and its implementation are
-- identical to the library-provided `genericServerT`, but it was left here
-- anyway in order to avoid modifying all use sites. We will do that later.
toServant ::
     forall m api. GenericServant api (AsServerT m)
  => api (AsServerT m)
  -> S.ToServant api (AsServerT m)
toServant = genericServerT