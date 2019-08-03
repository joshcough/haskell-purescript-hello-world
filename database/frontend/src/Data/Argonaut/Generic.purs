
-- | This module provides Generic.Rep-based JSON implementations that match those
-- | found in Haskell. More specifically, we look for the following behavior:
-- |
-- |    1. Parameterless DUs (aka "enums") are serialized as plain strings.
-- |       For example:
-- |
-- |           data E = A | B
-- |           genericEncodeJson A == "\"A\""
-- |           genericEncodeJson B == "\"B\""
-- |
-- |    2. Parameterless DUs with only one case are serialized as an empty array
-- |       (i.e. same as unit). For example:
-- |
-- |           data T = T
-- |           genericEncodeJson T == "[]"
-- |
-- |    3. Regular DUs get serialized as { tag: 'Ctor', contents: [1,2,3] }, with
-- |       a few special cases (see items below).
-- |       For example:
-- |
-- |           data T = A Int | B Int Boolean | C { x :: Int }
-- |           genericEncodeJson (A 42) = "{ \"tag\": \"A\", \"contents\": 42 }"
-- |           genericEncodeJson (B 42 true) = "{ \"tag\": \"B\", \"contents\": [42, true] }"
-- |           genericEncodeJson (C { x: 42 }) = "{ \"tag\": \"C\", \"contents\": { \"x\": 42 } }"
-- |
-- |    4. For single-constructor DUs, the constructor is thrown away before
-- |       serialization. For example:
-- |
-- |           data T a = T a
-- |           data U = U Int Boolean
-- |           genericEncodeJson (T 42) == "42"
-- |           genericEncodeJson (T "abc") == "\"abc\""
-- |           genericEncodeJson (T { x: 42, y: true }) == "{ \"x\": 42, \"y\": true }"
-- |           genericEncodeJson (U 42 true) == "[42, true]"
-- |
-- |    5. For DUs constructors that wrap a single record, the `tag` is included in the
-- |       record itself, without an extra `contents` level. For example:
-- |
-- |           data T = A { x :: Int } | B { y :: String }
-- |           genericEncodeJson (A {x: 42}) == "{ \"tag\": \"A\", \"x\": 42 }"
-- |           genericEncodeJson (B {y: "abc"}) == "{ \"tag\": \"B\", \"y\": \"abc\" }"
-- |
-- |    6. Maybe-typed record fields get unwraped from the Maybe and treated as optional.
-- |       For example:
-- |
-- |           data T = T { x :: Int, y :: Maybe Boolean }
-- |           genericDecodeJson "{ \"x\": 42 }" == T { x: 42, y: Nothing }
-- |           genericDecodeJson "{ \"x\": 42, \"y\": true }" == T { x: 42, y: Just true }
-- |
-- | To my great astonishment, I couldn't find an existing codec library that has
-- | these properties. Specifically, both purescript-argonaut-generic and
-- | purescript-foreign-generic don't hold up.
-- |
module Data.Argonaut.Generic(
    class GenericEncodeJson, genericEncodeJson,
    class GenericDecodeJson, genericDecodeJson
) where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut as Json
import Data.Argonaut.Generic.Decode (class DecodeRep, class DecodeSingleConstructor, decodeSingleConstructor, decodeAsEnum, decodeRep, orFail)
import Data.Argonaut.Generic.Encode (class EncodeRep, encodeAsEnum, encodeRep)
import Data.Argonaut.Generic.Utils (class IsEnum, class IsSingleConstructor, isEnum, isSingleConstructor)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, from, to)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Foreign.Object as Obj
import Type.Proxy (Proxy(..))


class GenericEncodeJson a where
    genericEncodeJson :: a -> Json
instance genericEncodeJsonInstance :: (
    Generic a rep,
    IsEnum rep,
    IsSingleConstructor rep,
    EncodeRep rep)
    =>
    GenericEncodeJson a
  where
    genericEncodeJson a | isEnum (Proxy :: Proxy rep) && isSingleConstructor (Proxy :: Proxy rep) =
        Json.fromArray []
    genericEncodeJson a | isEnum (Proxy :: Proxy rep) =
        Json.fromString $ encodeAsEnum $ from a
    genericEncodeJson a | Just enc <- encodeRep (from a) =
        case enc.contents of
            List.Nil ->
                mkObj [tag]
            List.Cons cnts List.Nil | isSingleConstructor (Proxy :: Proxy rep) ->
                cnts
            List.Cons cnts List.Nil
                | enc.isSingleRecordArg
                , Just obj <- Json.toObject cnts
              ->
                -- Special case for single-record-wrapping constructors: insert `tag` right in the record.
                Json.fromObject $ Obj.insert "tag" (snd tag) obj
            List.Cons cnts List.Nil ->
                mkObj [tag, Tuple "contents" cnts]
            lst | isSingleConstructor (Proxy :: Proxy rep) ->
                Json.fromArray $ List.toUnfoldable lst
            lst ->
                mkObj [tag, Tuple "contents" $ Json.fromArray $ List.toUnfoldable lst]

        where
            tag = Tuple "tag" $ Json.fromString enc.tag
            mkObj = Json.fromObject <<< Obj.fromFoldable
    genericEncodeJson _ =
        Json.jsonNull


class GenericDecodeJson a where
    genericDecodeJson :: Json -> Either String a
instance genericDecodeJsonInstance :: (
    Generic a rep,
    IsEnum rep,
    IsSingleConstructor rep,
    DecodeSingleConstructor rep,
    DecodeRep rep)
    =>
    GenericDecodeJson a
  where
    genericDecodeJson j | isSingleConstructor (Proxy :: Proxy rep) =
        to <$> decodeSingleConstructor j
    genericDecodeJson j | isEnum (Proxy :: Proxy rep) = do
        str <- Json.toString j `orFail` "Expected a string"
        rep <- decodeAsEnum str `orFail` ("Unknown constructor: '" <> str <> "'")
        pure $ to rep
    genericDecodeJson j = do
        obj <- Json.toObject j `orFail` "genericDecodeJson expected an object"
        tag <- (Json.toString =<< Obj.lookup "tag" obj) `orFail` "No 'tag' field in object"
        decoder <- decodeRep {tag} `orFail` ("Unknown constructor '" <> tag <> "'")
        let prefix = "Parsing '" <> tag <> "': "

        contents <- case decoder.expectedArgs of
            0 ->
                Right List.Nil
            1 | decoder.isSingleRecordArg ->
                -- Special case for single-record-wrapping constructors: look for `tag` right in the record.
                Right $ List.singleton (Json.fromObject obj)
            1 ->
                List.singleton <$> Obj.lookup "contents" obj `orFail` (prefix <> "No 'contents' field in object")
            n -> do
                contents <- Obj.lookup "contents" obj `orFail` (prefix <> "No 'contents' field in object")
                arr <- Json.toArray contents `orFail` (prefix <> "Expected 'contents' field to be an array")
                if length arr == n
                    then Right $ List.fromFoldable arr
                    else Left (prefix <> "Expected " <> show n <> " arguments, but got " <> show (length arr))

        lmap (prefix <> _) $ to <$> decoder.decode contents
