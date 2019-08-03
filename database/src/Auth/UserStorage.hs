module Auth.UserStorage
  ( UserDb(..)
  ) where

import Control.Monad.Except (MonadIO, liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import Protolude

import Auth.DatabaseModels (DbUser(..), DbUserId)
import qualified Auth.DatabaseModels as Db
import Auth.Models (CreateUser(..), User(..))
import Scaffolding.Types (AppT, runDb)
import Scaffolding.Config (Config)

class Monad m => UserDb m
  where
  getUserById :: DbUserId -> m (Maybe User)
  getUserByUsername :: Text -> m (Maybe User)
  getUserByEmail :: Text -> m (Maybe (User, Text))
  deleteUserById :: DbUserId -> m ()
  createUser :: CreateUser -> m (Maybe DbUserId)

-- runDb :: (HasConfig c, MonadReader c m, MonadIO m) => SqlPersistT IO b -> m b
instance MonadIO m => UserDb (AppT Config m) where
  getUserById = runDb . getUserById
  getUserByUsername = runDb . getUserByUsername
  getUserByEmail = runDb . getUserByEmail
  deleteUserById = runDb . deleteUserById
  createUser = runDb . createUser

instance MonadIO m => UserDb (SqlPersistT m) where
  getUserById = fmap (fmap entityToUser) . getEntity
  getUserByUsername username = fmap entityToUser <$> selectFirst [Db.DbUserName P.==. username] []
  getUserByEmail email = fmap f <$> selectFirst [Db.DbUserEmail P.==. email] []
    where
      f e@(Entity _ dbUser) = (entityToUser e, dbUserHashedPassword dbUser)
  deleteUserById = P.deleteCascade
  createUser (CreateUser name email pw) =
    liftIO (encryptPassword pw) >>= \case
      Nothing -> return Nothing
      Just password' -> do
        newUser <- insert $ DbUser name email (decodeUtf8 password')
        return . Just $ newUser
    where
      encryptPassword :: Text -> IO (Maybe ByteString)
      encryptPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy . encodeUtf8

-- |
entityToUser :: Entity DbUser -> User
entityToUser (Entity k DbUser {..}) = User (fromSqlKey k) dbUserName dbUserEmail