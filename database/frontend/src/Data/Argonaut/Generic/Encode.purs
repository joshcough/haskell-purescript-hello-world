
module Data.Argonaut.Generic.Encode where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object as Obj
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Rec
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

type Encoded = {
    tag :: String,
    isSingleRecordArg :: Boolean,
    contents :: List Json
}

class EncodeRep rep where
    encodeAsEnum :: rep -> String
    encodeRep :: rep -> Maybe Encoded
instance encNakedConstructor :: IsSymbol name  => EncodeRep (Constructor name NoArguments) where
    encodeAsEnum _ = reflectSymbol (SProxy :: SProxy name)
    encodeRep = encodeCtor
else instance encOtherConstructor :: (IsSymbol name, EncodeArgs a) => EncodeRep (Constructor name a) where
    encodeAsEnum _ = ""
    encodeRep = encodeCtor
instance encSum :: (EncodeRep a, EncodeRep b) => EncodeRep (Sum a b) where
    encodeAsEnum (Inl a) = encodeAsEnum a
    encodeAsEnum (Inr b) = encodeAsEnum b
    encodeRep (Inl a) = encodeRep a
    encodeRep (Inr b) = encodeRep b
instance encNoConsturctors :: EncodeRep NoConstructors where
    encodeAsEnum _ = ""
    encodeRep _ = Nothing

encodeCtor :: forall name a. IsSymbol name => EncodeArgs a => Constructor name a -> Maybe Encoded
encodeCtor (Constructor a) = Just {
    tag: reflectSymbol (SProxy :: SProxy name),
    isSingleRecordArg: isSingleRecordArg (Proxy :: Proxy a),
    contents: encodeArgs a
}

class EncodeArgs args where
    isSingleRecordArg :: Proxy args -> Boolean
    encodeArgs :: args -> List Json
instance encArgsEmpty :: EncodeArgs NoArguments where
    isSingleRecordArg _ = false
    encodeArgs _ = List.Nil
instance encArgsRec :: (RowToList r rl, EncodeRecordList r rl) => EncodeArgs (Argument (Record r)) where
    isSingleRecordArg _ = true
    encodeArgs (Argument r) = List.singleton $ Json.fromObject $ Obj.fromFoldable $ encodeRecordList r (RLProxy :: RLProxy rl)
else instance encArgsArgument :: Json.EncodeJson a => EncodeArgs (Argument a) where
    isSingleRecordArg _ = false
    encodeArgs (Argument a) = List.singleton $ Json.encodeJson a
instance encArgsProduct :: (EncodeArgs a, EncodeArgs b) => EncodeArgs (Product a b) where
    isSingleRecordArg _ = false
    encodeArgs (Product a b) = encodeArgs a <> encodeArgs b


-- | This class and its instances serve the sole purpose of treating
-- | `Maybe`-typed record fields in a special way: `Nothing` gets dropped
-- | from the record completely rather than being included as `null`.
class EncodeRecordField a where
    encodeRecordField :: a -> Maybe Json
instance decRecFieldMaybe :: Json.EncodeJson a => EncodeRecordField (Maybe a) where
    encodeRecordField = map Json.encodeJson
else instance decRecField :: Json.EncodeJson a => EncodeRecordField a where
    encodeRecordField = Just <<< Json.encodeJson


class EncodeRecordList r rl where
    encodeRecordList :: Record r -> RLProxy rl -> List (Tuple String Json)
instance encRListNil :: EncodeRecordList r Nil where
    encodeRecordList _ _ = List.Nil
instance encRListCons :: (
    IsSymbol name,
    EncodeRecordField a,
    EncodeRecordList r tail,
    Row.Cons name a r' r)
    =>
    EncodeRecordList r (Cons name a tail)
  where
    encodeRecordList r _ =
        case value of
            Just v -> List.Cons (Tuple name v) tail
            Nothing -> tail
        where
            name = reflectSymbol (SProxy :: SProxy name)
            value = encodeRecordField $ Rec.get (SProxy :: SProxy name) r
            tail = encodeRecordList r (RLProxy :: RLProxy tail)
