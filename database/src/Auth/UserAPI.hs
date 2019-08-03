module Auth.UserAPI where

import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Crypto.BCrypt (validatePassword)
import Data.Aeson (Value, (.=))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (fromSqlKey)
import Protolude
import Scaffolding.ServantHelpers
import Servant.Auth.Server

import Auth.DatabaseModels (DbUserId)
import Auth.Models (CreateUser(..), Login(..), User(..))
import qualified Auth.UserStorage as Db
import Scaffolding.Logging (logDebug)
import Scaffolding.Types (AppT, Config(..), HelloError(..))

type SetCookieHeader = Header "Set-Cookie" SetCookie

type SetCookieHeaders = '[ SetCookieHeader, SetCookieHeader]

---
--- Login API/Server
---
type LoginAPI = "login" :> Compose LoginServer

newtype LoginServer route =
  LoginServer
    { loginServerLogin :: route :- ReqBody '[JSON] Login :> Post '[JSON] (Headers SetCookieHeaders ())
    }
  deriving (Generic)

loginServer :: MonadIO m => ServerT LoginAPI (AppT Config m)
loginServer = toServant $ LoginServer login

{-
 - Here is the login handler. We do the following:
 - A) look up the user in the database by email addr, and throw 404 if not found
 - B) Check to see if they entered a valid password, and throw a 401 if not
 - C) Return the jwt token in the header.
 -}
login :: MonadIO m => Login -> AppT Config m (Headers SetCookieHeaders ())
login (Login e pw) = do
  maybeU <- Db.getUserByEmail e
  maybeOr401 maybeU $ \(user, hashedPw) -> guard401 (validate hashedPw) (applyCookies user)
  where
    validate hashedPw = validatePassword (encodeUtf8 hashedPw) (encodeUtf8 pw)

-- |
applyCookies :: (MonadError HelloError m, MonadIO m, MonadReader Config m) => User -> m (Headers SetCookieHeaders ())
applyCookies usr = do
  cookieSettings <- asks _configCookies
  jwtSettings <- asks _configJWT
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
  maybeOr401 mApplyCookies (\app -> return . app $ ())

-----
----- User API/Server
-----
--type UserAPI = "users" :> Compose UserServer
--
--data UserServer route =
--  UserServer
--    { userServerGetUserById :: route :- Capture "id" DbUserId :> Get '[JSON] User
--    -- , userServerDeleteUser :: route :- Capture "id" DbUserId :> Delete '[JSON] ()
--    , userServerCreateUser :: route :- ReqBody '[JSON] CreateUser :> Post '[JSON] DbUserId
--    }
--  deriving (Generic)
--
---- | The server that runs the UserAPI
--userServer :: (MonadIO m) => User -> ServerT UserAPI (AppT Config m)
--userServer caller = toServant $ UserServer {..}
--  where
--    userServerGetUserById = getUserById caller
--    -- userServerDeleteUser = deleteUser caller
--    userServerCreateUser = createUser caller
--
---- | Returns a user by name or throws a 404 error.
--getUserById :: MonadIO m => User -> DbUserId -> AppT Config m User
--getUserById caller uid = do
--  $(logDebug) "getUserById" ["uid" .= uid]
--  callerIsUserOr401 caller uid $ withUserOr404 uid return
--
------ | Creates a user in the database.
----deleteUser :: MonadIO m => User -> DbUserId -> AppT Config m ()
----deleteUser caller uid = do
----  $(logDebug) "deleteUser" ["uid" .= uid]
----  adminOr401 caller $ withUserOr404 uid (const $ Db.deleteUserById uid)
--
---- | Creates a user in the database.
--createUser :: MonadIO m => User -> CreateUser -> AppT Config m DbUserId
--createUser caller c = do
--  $(logDebug) "createUser" []
--  adminOr401 caller $ Db.createUser c >>= flip (maybeOr500 "Couldn't create user.") return
--
---- | Look up a user by id. If it exist, run an operation on it. If not, throw a 404.
--withUserOr404 :: MonadIO m => DbUserId -> (User -> AppT Config m b) -> AppT Config m b
--withUserOr404 uid m = Db.getUserById uid >>= flip maybeOr404 m