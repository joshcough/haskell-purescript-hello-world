module Data.Argonaut.Generic.Decode where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..))
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object as Obj
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder as RecBuilder
import Type.Data.RowList (RLProxy(..))
import Type.Proxy (Proxy(..))

type Decoder rep = {
    expectedArgs :: Int,
    isSingleRecordArg :: Boolean,
    decode :: List Json -> Either String rep
}

class DecodeRep rep where
    decodeAsEnum :: String -> Maybe rep
    decodeRep :: { tag :: String } -> Maybe (Decoder rep)
instance decNakedConstructor :: IsSymbol name => DecodeRep (Constructor name NoArguments) where
    decodeAsEnum s | s == reflectSymbol (SProxy :: SProxy name) = Just $ Constructor NoArguments
    decodeAsEnum _ = Nothing
    decodeRep = decodeCtor
else instance decOtherConstructor :: (IsSymbol name, DecodeArgs a) => DecodeRep (Constructor name a) where
    decodeAsEnum _ = Nothing
    decodeRep = decodeCtor
instance decSum :: (DecodeRep a, DecodeRep b) => DecodeRep (Sum a b) where
    decodeAsEnum s = (Inl <$> decodeAsEnum s) <|> (Inr <$> decodeAsEnum s)
    decodeRep tag = try Inl <|> try Inr
        where
            try :: forall x. DecodeRep x => (x -> Sum a b) -> Maybe (Decoder (Sum a b))
            try f = decodeRep tag <#> \d -> d { decode = map f <<< d.decode }
instance decNoConsturctors :: DecodeRep NoConstructors where
    decodeAsEnum _ = Nothing
    decodeRep _ = Nothing

decodeCtor :: forall name a. IsSymbol name => DecodeArgs a => { tag :: String } -> Maybe (Decoder (Constructor name a))
decodeCtor {tag} | tag == reflectSymbol (SProxy :: SProxy name) =
    Just {
        expectedArgs: countArgs (Proxy :: Proxy a),
        isSingleRecordArg: isSingleRecordArg (Proxy :: Proxy a),
        decode: \js -> do
            {args} <- decodeArgs js
            pure $ Constructor args
    }
decodeCtor _ =
    Nothing

class DecodeArgs args where
    countArgs :: Proxy args -> Int
    isSingleRecordArg :: Proxy args -> Boolean
    decodeArgs :: List Json -> Either String { args :: args, rest :: List Json }
instance decArgsEmpty :: DecodeArgs NoArguments where
    countArgs _ = 0
    isSingleRecordArg _ = false
    decodeArgs rest = Right { args: NoArguments, rest }
instance decArgsRec :: (RowToList r rl, DecodeRecordList r rl) => DecodeArgs (Argument (Record r)) where
    countArgs _ = 1
    isSingleRecordArg _ = true
    decodeArgs (List.Cons j List.Nil) = decodeRecord j <#> \r -> { args: Argument r, rest: List.Nil }
    decodeArgs _ = Left "Expected exactly one value"
else instance decArgsArgument :: Json.DecodeJson a => DecodeArgs (Argument a) where
    countArgs _ = 1
    isSingleRecordArg _ = false
    decodeArgs (List.Cons j rest) = { args: _, rest } <<< Argument <$> Json.decodeJson j
    decodeArgs _ = Left "Not enough arguments"
instance decArgsProduct :: (DecodeArgs a, DecodeArgs b) => DecodeArgs (Product a b) where
    countArgs _ = countArgs (Proxy :: Proxy a) + countArgs (Proxy :: Proxy b)
    isSingleRecordArg _ = false
    decodeArgs lst = do
        { args: a, rest: aRest } <- decodeArgs lst
        { args: b, rest: bRest } <- decodeArgs aRest
        pure { args: Product a b, rest: bRest }

decodeRecord :: forall r rl. RowToList r rl => DecodeRecordList r rl => Json -> Either String (Record r)
decodeRecord j = do
    obj <- Json.toObject j `orFail` ("decodeRecord expected an object, but got: " <> show (Json.toString j))
    builder <- decodeRecordList (RLProxy :: RLProxy rl) obj
    pure $ RecBuilder.build builder {}


-- | This class and its instances serve the sole purpose of treating
-- | `Maybe`-typed record fields in a special way: missing fields get
-- | interpreted as `Nothing` instead of raising an error.
class DecodeRecordField a where
    decodeRecordField :: Maybe Json -> Either String a
instance decRecFieldMaybe :: Json.DecodeJson a => DecodeRecordField (Maybe a) where
    decodeRecordField (Just j) = Json.decodeJson j
    decodeRecordField Nothing = Right Nothing
else instance decRecField :: Json.DecodeJson a => DecodeRecordField a where
    decodeRecordField (Just j) = Json.decodeJson j
    decodeRecordField Nothing = Left "missing field"


class DecodeRecordList r rl | rl -> r where
    decodeRecordList :: RLProxy rl -> Obj.Object Json -> Either String (RecBuilder.Builder {} (Record r))
instance decRListNil :: DecodeRecordList () Nil where
    decodeRecordList _ _ = Right identity
instance decRListCons :: (
    IsSymbol name,
    DecodeRecordField a,
    DecodeRecordList r' tail,
    Row.Cons name a r' r,
    Row.Lacks name r')
    =>
    DecodeRecordList r (Cons name a tail)
  where
    decodeRecordList _ obj = do
        val <- prefixErr $ decodeRecordField (Obj.lookup name obj)
        rest <- decodeRecordList (RLProxy :: RLProxy tail) obj
        pure $ RecBuilder.insert (SProxy :: SProxy name) val <<< rest
        where
            prefixErr = lmap \e -> "Parsing '" <> name <> "': " <> e
            name = reflectSymbol (SProxy :: SProxy name)


-- | See `IsSingleConstructor` in ./Utils.purs
class DecodeSingleConstructor rep where
    decodeSingleConstructor :: Json -> Either String rep
instance decSingleRecord :: (RowToList r rl, DecodeRecordList r rl) => DecodeSingleConstructor (Constructor name (Argument (Record r))) where
    decodeSingleConstructor j = Constructor <<< Argument <$> decodeRecord j
else instance decSingleMultiArg :: DecodeArgs a => DecodeSingleConstructor (Constructor name a) where
    decodeSingleConstructor j = do
        jArgs <- case countArgs (Proxy :: Proxy a) of
            0 | Just [] <- Json.toArray j -> Right List.Nil
            0 -> Left "Expected an empty array"
            1 -> Right $ List.singleton j
            n -> List.fromFoldable <$> Json.toArray j `orFail` "Expected an array"
        Constructor <<< _.args <$> decodeArgs jArgs
instance decSingleSum :: DecodeSingleConstructor (Sum a b) where
    decodeSingleConstructor _ = Left "Not a single constructor"
instance decSingleEmpty :: DecodeSingleConstructor NoConstructors where
    decodeSingleConstructor _ = Left "Not a single constructor"


orFail :: forall a. Maybe a -> String -> Either String a
orFail f s = maybe (Left s) Right f
