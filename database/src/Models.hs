{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models
  ( Message(..)
  ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Message =
  Message
    { _messageText :: Text
    }
  deriving  (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)