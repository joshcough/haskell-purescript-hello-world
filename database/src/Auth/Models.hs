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
    loginEmail    :: Text
  , loginPassword :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data User = User {
    userId       :: Int64
  , userName     :: Text
  , userEmail    :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToJWT, FromJWT)

data CreateUser = CreateUser {
    createUserName     :: Text
  , createUserEmail    :: Text
  , createUserPassword :: Text
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CreateUserResponse = CreateUserResponse {
    createUserResponse :: Either Text ()
} deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)