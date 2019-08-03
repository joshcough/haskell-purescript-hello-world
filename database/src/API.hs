module API
  ( app
  ) where

import Auth.Models (User(..))
import Auth.UserAPI (LoginAPI, loginServer)
import Control.Monad.Except (MonadIO, liftIO, throwError)
import Data.Aeson ((.=))
import Models (Message(..))
import Network.Wai (Application)
import Protolude
import Scaffolding.Config (Config(..))
import Scaffolding.Error (AppError(..), AuthError(..), throwAll, toServantErr)
import Scaffolding.Logging (logDebug, tShow)
import Scaffolding.ServantHelpers
import Scaffolding.Types (App, AppT, runAppT)
import Servant.Auth.Server hiding (throwAll)

type TopLevelAPI' auths = (Auth auths User :> Protected) :<|> Unprotected
type TopLevelAPI        = TopLevelAPI' '[Cookie, JWT]

type Protected = Compose ProtectedServer

-- | Lives behind authorization. Only logged in users can visit these pages.
newtype ProtectedServer route = ProtectedServer {
    protectedServerHelloWorld :: route :- HelloWorldAPI
  } deriving Generic

protectedServer :: MonadIO m => User -> ServerT Protected (AppT Config m)
protectedServer u = toServant $ ProtectedServer {..}
  where
  protectedServerHelloWorld = helloWorldServer u

type HelloWorldAPI = "api" :> Compose Api

newtype Api route =
  Api
    { hello :: route :- "hello" :> Get '[JSON] Message
    }
  deriving (Generic)

helloWorldServer :: (MonadIO m) => User -> ServerT HelloWorldAPI (AppT Config m)
helloWorldServer u = toServant $ Api {..}
  where
    hello = do
      let m = Message ("Hello, World" <> userName u)
      $(logDebug) "in hello function" ["message" .= tShow m]
      pure m

type Unprotected = Compose UnprotectedServer

-- | Not protected by any authorization. Anyone can visit these pages.
newtype UnprotectedServer route = UnprotectedServer {
    unprotectedServerLoginApi :: route :- LoginAPI
  } deriving Generic

-- |
unprotectedServer :: (MonadIO m) => ServerT Unprotected (AppT Config m)
unprotectedServer = toServant $ UnprotectedServer {..}
    where
    unprotectedServerLoginApi = loginServer

-- | The main application for the Proverlays backend.
app :: Config -> Application
app cfg = serveWithContext
            (Proxy :: Proxy (TopLevelAPI :<|> Raw))
            (_configCookies cfg :. _configJWT cfg :. EmptyContext)
            (mainServer :<|> serveDirectoryFileServer "frontend")
    where
    convertApp :: Config -> App a -> Handler a
    convertApp cfg' appt = Handler $
        liftIO (runAppT appt cfg') >>= either (throwError . toServantErr) return

    protectedServer' (Authenticated u) = protectedServer u
    protectedServer' _ = throwAll (AppAuthError NoAuthError)

    mainServer :: Server TopLevelAPI
    mainServer = hoistServerWithContext
        (Proxy :: Proxy TopLevelAPI)
        (Proxy :: Proxy '[CookieSettings, JWTSettings])
        (convertApp cfg)
        (protectedServer' :<|> unprotectedServer)
