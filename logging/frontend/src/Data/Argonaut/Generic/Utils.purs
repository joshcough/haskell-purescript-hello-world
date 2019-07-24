module Data.Argonaut.Generic.Utils where

import Prelude
import Data.Generic.Rep (Constructor, NoArguments, NoConstructors, Sum)
import Type.Proxy (Proxy(..))

-- | An "enum" is a DU that has one or more constructors, all of which
-- | are parameterless. This type function is used to enable a special-case
-- | serialization of such types (see ../Generic.purs)
class IsEnum rep where
    isEnum :: Proxy rep -> Boolean
instance isEnumNakedConstructor :: IsEnum (Constructor name NoArguments) where
    isEnum _ = true
else instance isEnumOtherConstructor :: IsEnum (Constructor name a) where
    isEnum _ = false
instance isEnumSum :: (IsEnum a, IsEnum b) => IsEnum (Sum a b) where
    isEnum _ = isEnum (Proxy :: Proxy a) && isEnum (Proxy :: Proxy b)
instance isEnumNoConsturctors :: IsEnum NoConstructors where
    isEnum _ = false

-- | An "single-constrctor type" is a DU that has exactly one constructor.
-- | This type function is used to enable a special-case serialization of
-- | such types (see ../Generic.purs)
class IsSingleConstructor rep where
    isSingleConstructor :: Proxy rep -> Boolean
instance isSingle :: IsSingleConstructor (Constructor name a) where
    isSingleConstructor _ = true
instance isSingleSum :: IsSingleConstructor (Sum a b) where
    isSingleConstructor _ = false
instance isSingleEmpty :: IsSingleConstructor NoConstructors where
    isSingleConstructor _ = false
