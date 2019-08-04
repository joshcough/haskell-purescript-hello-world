{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Auth.Models (
    CreateUser(..)
  , CreateUserResponse(..)
  , Login(..)
  , User(..)
  ) where

import Protolude
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Int               (Int64)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Servant.Auth.Server

data Login = Login {
    _loginEmail    :: Text
  , _loginPassword :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User {
    _userId       :: Int64
  , _userName     :: Text
  , _userEmail    :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

data CreateUser = CreateUser {
    _createUserName     :: Text
  , _createUserEmail    :: Text
  , _createUserPassword :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CreateUserResponse = CreateUserResponse {
    _createUserResponse :: Either Text ()
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)