module Network.HTTP(
    module Data.HTTP.Method,
    module Affjax.RequestHeader,
    class AsHttpException,
    HttpException(..),
    Request,
    asHttpException,
    buildReq,
    addHeaders,
    http,
    httpFriendly,
    httpJSON,
    httpJSONFriendly,
    noData,
    jsonData,
    nothingOn404
) where

import Prelude

import Affjax as Affjax
import Affjax.RequestBody as Request
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (intercalate)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, message)

class AsHttpException e where
    asHttpException :: HttpException -> e

instance asHttpExceptionHttpException :: AsHttpException HttpException where
    asHttpException = identity

-- We always accept response as a plain string and then try to
-- parse (or ignore) it ourselfves. This is necessary, because
-- we also want to catch the case when incoming JSON is broken
-- or there is error text in the body.
type Request = Affjax.Request String

data HttpException
    = NetworkException Error Request
    | StatusCodeException Int Request
    | UserFriendlyStatusException String
    | ResponseParseException { error :: String, originalJson :: String }

instance showHttpException :: Show HttpException where
    show (NetworkException e req) = "NetworkException " <> message e <> "\n\n" <> showRequest req
    show (StatusCodeException code req) = "StatusCodeException " <> show code <> "\n\n" <> showRequest req
    show (ResponseParseException e) = "ResponseParseException '" <> e.error <> "' when parsing " <> e.originalJson
    show (UserFriendlyStatusException e) = "Unexpected response status code"

showRequest :: Request -> String
showRequest {method, url, headers} =
    showMethod method <> " " <> url <> "\n" <>
    showHeaders
    where
        showMethod (Left a) = show a  -- Standard HTTP verbs.
        showMethod (Right a) = show a  -- Custom HTTP verbs, we don't use those, but types demand it.
        showHeader (RequestHeader h v) = h <> ": " <> v
        showHeader (Accept v) = "Accept: " <> show v
        showHeader (ContentType v) = "ContentType: " <> show v
        showHeaders = intercalate "\n" (showHeader <$> headers)


buildReq :: Method -> String -> Maybe Request.RequestBody -> Request
buildReq mthd url dta = {
    method: Left mthd,
    url: url,
    headers: [],
    content: dta,
    username: Nothing,
    password: Nothing,
    withCredentials: false,
    responseFormat: Response.string
}

addHeaders :: Array RequestHeader -> Request -> Request
addHeaders hx r = r { headers = r.headers <> hx }


type InvokeRequest m r = forall ex. MonadAff m => MonadError ex m => AsHttpException ex => Request -> m r

http :: forall m. InvokeRequest m Unit
http = httpSafe hideUglyError ignore

httpJSON :: forall m r. DecodeJson r => InvokeRequest m r
httpJSON = httpSafe hideUglyError asJSON

-- The difference between `http` and `httpFriendly` is that former assumes the error messages
-- that are returned from the server are ugly and need to be hidden behind a generic
-- "something's wrong" message, but the latter assumes that the server-generated messages are
-- friendly (hence the name) and passes them on to the consumer.
-- Ideally, all our server-generated messages ought to be friendly like this, but right now
-- they're not, so we have two function variants to handle that.
httpFriendly :: forall m. InvokeRequest m Unit
httpFriendly = httpSafe passFriendlyError ignore

-- See comment on `httpFriendly` above.
httpJSONFriendly :: forall m r. DecodeJson r => InvokeRequest m r
httpJSONFriendly = httpSafe passFriendlyError asJSON


httpSafe :: forall m r.
    (Request -> StatusCode -> String -> HttpException)
    -> (String -> Either HttpException r)
    -> InvokeRequest m r
httpSafe toError toResult req =
    either (throwError <<< asHttpException) pure
    =<< httpSafe' toError toResult req

httpSafe' :: forall m r.
    (Request -> StatusCode -> String -> HttpException)
    -> (String -> Either HttpException r)
    -> InvokeRequest m (Either HttpException r)
httpSafe' toError toResult req =
    liftAff $ (onSuccess <$> Affjax.request req) `catchError` onError
    where
        onError e = pure $ Left $ NetworkException e req

        onSuccess response = case response.body of
            Left err@(Affjax.ResponseFormatError _ f) ->
                Left $ ResponseParseException {
                    error: Affjax.printResponseFormatError err,
                    originalJson: ""
                }
            Right dta | StatusCode code <- response.status, code < 300 ->
                toResult dta
            Right dta ->
                Left $ toError req response.status dta


-- For all requests ignores their body, generates an exception
-- without a friendly message. See comment on `httpFriendly` above.
hideUglyError :: Request -> StatusCode -> String -> HttpException
hideUglyError req (StatusCode c) _ = StatusCodeException c req

-- Passes on the friendly error message from the server (see comment
-- on `httpFriendly` above), except for 403, in which case server
-- response remains hidden.
passFriendlyError :: Request -> StatusCode -> String -> HttpException
passFriendlyError req (StatusCode 403) _ = StatusCodeException 403 req
passFriendlyError _ _ msg = UserFriendlyStatusException msg


-- Response-converting function for use with `httpSafe`.
-- Attempts to decode from JSON.
asJSON :: forall r. DecodeJson r => String -> Either HttpException r
asJSON s = case decodeJson =<< jsonParser s of
    Left e -> Left $ ResponseParseException { error: e, originalJson: s }
    Right r -> Right r

-- Response-converting function for use with `httpSafe`.
-- Ignores the response.
ignore :: String -> Either HttpException Unit
ignore _ = Right unit


noData :: Maybe Request.RequestBody
noData = Nothing

jsonData :: forall a. EncodeJson a => a -> Maybe Request.RequestBody
jsonData = Just <<< Request.json <<< encodeJson


nothingOn404 :: forall m r ex.
    MonadAff m => MonadError ex m => AsHttpException ex => DecodeJson r
    => Request
    -> m (Maybe r)
nothingOn404 req = do
    res <- httpSafe' hideUglyError asJSON req
    case res of
        Left (StatusCodeException 404 _) -> pure Nothing
        Left ex -> throwError $ asHttpException ex
        Right r -> pure $ Just r
