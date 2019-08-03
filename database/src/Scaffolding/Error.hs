{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Scaffolding.Error(
    AuthError(..),
    TitledError(..),
    ToServant(..),
    ClassifiedError(..),
    AppError(..),
    ThrowAll(..),
    HelloError,
    HelloError'(..),
    catchAuth,
    throwToIO,
    defaultToRollbarEvent,
    orFail,
    orFailM,
    miscError
) where

import Protolude
import Control.Exception (Exception)
import Control.Lens (prism, makeClassyPrisms)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Network.HTTP.Nano (AsHttpError(..), HttpError)
import Servant ((:<|>) (..), ServantErr(..), err400, err401, err403, err404, err409, err500)
import Web.Rollbar (Event(..), EventLevel(..), ToRollbarEvent(..))

---
---
---

data AuthError
    = NoAuthError
    | BadAuthError
    deriving Show

makeClassyPrisms ''AuthError

---
---
---

class TitledError e where
    getErrorTitle :: e -> Text

class ToServant e where
    toServantErr :: e -> ServantErr

instance ToServant () where
    toServantErr _ = err500

class ClassifiedError e where
    -- An exception is either expected (thrown intentionally
    -- as a result of normal control flow), or unexpected (
    -- thrown outside of local control flow, and not explicitly
    -- handled). Expected exceptions are not logged to Rollbar;
    -- that should be done by the requestor, to whom the exception
    -- is unexpected. Unexpected exceptions are logged to Rollbar.
    isUnexpected :: e -> Bool
    isUnexpected  _ = True

data AppError e
    = AppHttpError HttpError
    | AppBadRequestError Text
    | AppAuthError AuthError
    | AppNotFoundError Text
    | AppConflictError Text
    | AppUnexpectedError Text
    | AppAppError e
    deriving (Show, Functor, Foldable, Traversable)

makeClassyPrisms ''AppError

type HelloError = AppError HelloError'

newtype HelloError'
    = HelloMiscError Text
    deriving (Show, Eq)

miscError :: Text -> HelloError
miscError = AppAppError . HelloMiscError

instance ClassifiedError HelloError' where
    isUnexpected (HelloMiscError _) = True

instance TitledError HelloError' where
    getErrorTitle (HelloMiscError _) = "Unknown Error"

instance ToServant HelloError' where
    toServantErr (HelloMiscError _) = err500

instance Exception e => Exception (AppError e)

instance ClassifiedError e => ClassifiedError (AppError e) where
    isUnexpected (AppAuthError _) = False
    isUnexpected (AppBadRequestError _) = False
    isUnexpected (AppNotFoundError _) = False
    isUnexpected (AppConflictError _) = False
    isUnexpected (AppAppError e) = isUnexpected e
    isUnexpected _ = True

instance AsHttpError (AppError e) where
    _HttpError = prism AppHttpError asHttpError

asHttpError :: AppError e -> Either (AppError e) HttpError
asHttpError (AppHttpError e) = Right e
asHttpError e = Left e

instance TitledError () where
    getErrorTitle = const "()"

instance ClassifiedError () where
    isUnexpected _ = True

instance TitledError e => TitledError (AppError e) where
    getErrorTitle (AppHttpError _) = "HTTP Error"
    getErrorTitle (AppBadRequestError _) = "Bad Request"
    getErrorTitle (AppAuthError NoAuthError) = "No Auth"
    getErrorTitle (AppAuthError BadAuthError) = "Bad Auth"
    getErrorTitle (AppNotFoundError _) = "Not Found"
    getErrorTitle (AppConflictError _) = "Conflict"
    getErrorTitle (AppUnexpectedError _) = "Unexpected"
    getErrorTitle (AppAppError e) = "App Error: " <> getErrorTitle e

instance ToServant e => ToServant (AppError e) where
    toServantErr (AppHttpError _) = err500
    toServantErr (AppBadRequestError e) = err400 { errBody = BL.pack $ unpack e }
    toServantErr (AppAuthError NoAuthError) = err401
    toServantErr (AppAuthError BadAuthError) = err403
    toServantErr (AppNotFoundError e) = err404 { errBody = BL.pack $ unpack e }
    toServantErr (AppConflictError e) = err409 { errBody = BL.pack $ unpack e }
    toServantErr (AppUnexpectedError _) = err500 -- TODO: use string or?
    toServantErr (AppAppError e) = toServantErr e

instance (TitledError e, Show e) => ToRollbarEvent (AppError e) where
    toRollbarEvent = defaultToRollbarEvent

defaultToRollbarEvent :: (TitledError e, Show e) => e -> Event
defaultToRollbarEvent e =
    Event {
        _eventLevel = Error,
        _eventUUID = Nothing,
        _eventTitle = getErrorTitle e,
        _eventMessage = tShow e,
        _eventData = Nothing,
        _eventContext = Nothing
    }

--
--
--

catchAuth :: MonadError (AppError e) m => m a -> (AuthError -> m a) -> m a
catchAuth f ef = catchError f handleAuth
    where
    handleAuth (AppAuthError e) = ef e
    handleAuth e = throwError e

throwToIO :: (Exception e, MonadIO m) => e -> m a
throwToIO = liftIO . throwIO

orFail :: MonadError err m => Maybe a -> err -> m a
orFail (Just a) _ = pure a
orFail Nothing err = throwError err

orFailM :: MonadError err m => m (Maybe a) -> err -> m a
orFailM m err = flip orFail err =<< m


---
---
---

class ThrowAll a where
  -- | 'throwAll' is a convenience function to throw errors across an entire
  -- sub-API
  --
  --
  -- > throwAll err400 :: Handler a :<|> Handler b :<|> Handler c
  -- >    == throwError err400 :<|> throwError err400 :<|> err400
  throwAll :: HelloError -> a

instance (ThrowAll a, ThrowAll b) => ThrowAll (a :<|> b) where
  throwAll e = throwAll e :<|> throwAll e

-- Really this shouldn't be necessary - ((->) a) should be an instance of
-- MonadError, no?
instance {-# OVERLAPPING #-} ThrowAll b => ThrowAll (a -> b) where
  throwAll e = const $ throwAll e

instance {-# OVERLAPPABLE #-} (MonadError HelloError m) => ThrowAll (m a) where
  throwAll = throwError

-- |
tShow :: Show a => a -> Text
tShow = pack . show
