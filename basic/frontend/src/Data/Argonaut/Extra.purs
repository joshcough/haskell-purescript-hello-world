module Data.Argonaut.Extra
    ( withObject
    ) where

import Data.Argonaut (Json, toObject)
import Data.Either (Either(Left))
import Data.Maybe (maybe)
import Foreign.Object (Object)

withObject :: forall a. String -> (Object Json -> Either String a) -> Json -> Either String a
withObject expected f json = maybe (Left expected) f (toObject json)
