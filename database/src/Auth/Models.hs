{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Auth.Models (
    CreateUser(..)
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
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

data User = User {
    userId       :: Int64
  , userName     :: Text
  , userEmail    :: Text
} deriving (Eq, Generic, Show, ToJSON, FromJSON, ToJWT, FromJWT)

data CreateUser = CreateUser {
    createUserName     :: Text
  , createUserEmail    :: Text
  , createUserPassword :: Text
} deriving (Eq, Generic, Show, ToJSON, FromJSON)
