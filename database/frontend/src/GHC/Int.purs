module GHC.Int (
    Int64(..)
) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, encodeJson)
import Data.Argonaut.Generic (genericDecodeJson, genericEncodeJson)

newtype Int64 = Int64 Int

derive instance eqInt64 :: Eq Int64
derive instance ordInt64 :: Ord Int64
derive instance genericInt64 :: Generic Int64 _

instance decodeInt64 :: DecodeJson Int64 where
    decodeJson j = Int64 <$> decodeJson j
instance encodeInt64 :: EncodeJson Int64 where
    encodeJson (Int64 i) = encodeJson i
