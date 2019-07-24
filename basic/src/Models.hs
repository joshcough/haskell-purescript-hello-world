{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models
  ( Message(..)
  ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Message =
  Message
    { _messageText :: Text
    , _messageInt :: Int
    , _messageList :: [Int]
    }
  deriving  (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)