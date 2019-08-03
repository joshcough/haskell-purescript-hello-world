module Scaffolding.ServantHelpers
  ( module Scaffolding.Config
  , module Servant
  , module Servant.API.Generic
  , module Servant.Server.Generic
  , Compose
  , toServant
  , guard401
  , maybeOr401
  , maybeOr404
  , maybeOr500
  , unexpected
  , callerIsUserOr401
  ) where

import Protolude

import Auth.DatabaseModels (DbUserId)
import Auth.Models (User(..))
import Control.Monad.Except (throwError)
import Database.Persist.Postgresql (fromSqlKey)
import Scaffolding.Config (Config(..))
import Scaffolding.Error (AppError(..), AuthError(..), HelloError, HelloError'(..))
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

-- |
unexpected :: MonadError HelloError m => Text -> m a
unexpected = throwError . AppAppError . HelloMiscError

-- |
guard401 :: MonadError (AppError e) m => Bool -> m a -> m a
guard401 b m =
  if b
    then m
    else throwError $ AppNotFoundError ""

-- |
maybeOr404 :: MonadError (AppError e) m => Maybe a -> (a -> m b) -> m b
maybeOr404 = maybeOrErr $ AppNotFoundError ""

-- |
maybeOr401 :: MonadError (AppError e) m => Maybe a -> (a -> m b) -> m b
maybeOr401 = maybeOrErr (AppAuthError NoAuthError)

-- |
maybeOr500 :: MonadError (AppError e) m => Text -> Maybe a -> (a -> m b) -> m b
maybeOr500 msg = maybeOrErr (AppUnexpectedError msg)

-- |
maybeOrErr :: MonadError (AppError e) m => AppError e -> Maybe a -> (a -> m b) -> m b
maybeOrErr err = flip $ maybe (throwError err)

-- |
callerIsUserOr401 :: MonadError (AppError e) m => User -> DbUserId -> m a -> m a
callerIsUserOr401 caller uid m = orNoAuth m $ userId caller == fromSqlKey uid

-- |
orNoAuth :: MonadError (AppError e) m => m a -> Bool -> m a
orNoAuth m b = if b then m else throwError (AppAuthError NoAuthError)