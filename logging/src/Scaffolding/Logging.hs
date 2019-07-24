{-# LANGUAGE UndecidableInstances #-}

module Scaffolding.Logging
    ( -- * Logging with metadata
      logDebug
    , logInfo
    , logWarn
    , logError
    , logOther
    -- * Logging without metadata
    , logDebug_
    , logInfo_
    , logWarn_
    , logError_
    , logOther_
    -- * Monad
    , MonadLoggerJSON
    , LoggingJSONT
    , runStdoutLoggingJSONT
    , runStdoutLoggingJSONTVerbose
    -- * Transformers
    , filterLogger
    -- * 'Control.Monad.Logger'
    , LogLevel(..)
    , runNoLoggingT
    , LoggingCfg(..)
    , HasLoggingCfg
    , loggingCfg
    , fromEnv
    , logLevel
    , logSourceVersion
    , SourceVersion(..)
    , tShow
    ) where

import Protolude hiding (catch, lift)

import Control.Lens (makeClassy)
import Control.Monad (when)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO(..), withUnliftIO, UnliftIO(..))
import Control.Monad.Logger (Loc(..), LogLevel(..), LogStr, liftLoc, toLogStr, runNoLoggingT)
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Aeson (ToJSON(), Value, (.=), object, toJSON)
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), defConfig, encodePretty', keyOrder)
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime, getCurrentTime)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (lift, qLocation)
import System.Environment (lookupEnv)
import System.IO (Handle, stdout)
import System.Log.FastLogger (fromLogStr)
import Text.Read (readMaybe)

-- Required for vendored instances from:
-- https://www.stackage.org/haddock/lts-10.10/monad-logger-0.3.28.2/src/Control.Monad.Logger.html#line-518
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

newtype SourceVersion = SourceVersion Text
  deriving (Show, Eq, Ord)

instance ToJSON SourceVersion where
    toJSON (SourceVersion s) = toJSON s

data LoggingCfg = LoggingCfg
    { _logLevel :: !LogLevel
    , _logSourceVersion :: !(Maybe SourceVersion)
    }
    deriving (Eq, Ord, Show)

fromEnv :: IO LoggingCfg
fromEnv = do
    mRawLevel <- lookupEnv "LOG_LEVEL"
    sourceVersion <- (fmap . fmap) (SourceVersion . T.pack) (lookupEnv "SOURCE_VERSION")
    return $ LoggingCfg
        { _logLevel = fromMaybe LevelDebug (mRawLevel >>= readMaybe)
        , _logSourceVersion = sourceVersion
        }

makeClassy ''LoggingCfg

class Monad m => MonadLoggerJSON m where
    monadLoggerJSONLog :: Loc -> LogLevel -> Text -> [Pair] -> m ()

newtype LoggingJSONT m a = LoggingJSONT
    { runLoggingJSONT :: (Loc -> LogLevel -> LogStr -> [Pair] -> IO ()) -> m a
    }

-- Instances vendored from:
-- https://www.stackage.org/haddock/lts-10.10/monad-logger-0.3.28.2/src/Control.Monad.Logger.html#line-518
instance Functor m => Functor (LoggingJSONT m) where
    fmap f logger = LoggingJSONT $ fmap f . runLoggingJSONT logger
    {-# INLINE fmap #-}

instance Applicative m => Applicative (LoggingJSONT m) where
    pure = LoggingJSONT . const . pure
    {-# INLINE pure #-}
    loggerF <*> loggerA =
        LoggingJSONT $ \loggerFn ->
            runLoggingJSONT loggerF loggerFn <*> runLoggingJSONT loggerA loggerFn
    {-# INLINE (<*>) #-}

instance Monad m => Monad (LoggingJSONT m) where
    return = LoggingJSONT . const . return
    LoggingJSONT ma >>= f =
        LoggingJSONT $ \r -> do
            a <- ma r
            let LoggingJSONT f' = f a
            f' r

instance MonadIO m => MonadIO (LoggingJSONT m) where
    liftIO = Trans.lift . liftIO

instance MonadThrow m => MonadThrow (LoggingJSONT m) where
    throwM = Trans.lift . throwM

instance MonadCatch m => MonadCatch (LoggingJSONT m) where
    catch (LoggingJSONT m) c = LoggingJSONT $ \r -> m r `catch` \e -> runLoggingJSONT (c e) r

instance MonadBase b m => MonadBase b (LoggingJSONT m) where
    liftBase = Trans.lift . liftBase

instance Trans.MonadTrans LoggingJSONT where
    lift = LoggingJSONT . const

instance MonadBaseControl b m => MonadBaseControl b (LoggingJSONT m) where
    type StM (LoggingJSONT m) a = StM m a
    liftBaseWith f =
        LoggingJSONT $ \reader' ->
            liftBaseWith $ \runInBase -> f $ runInBase . (\(LoggingJSONT r) -> r reader')
    restoreM = LoggingJSONT . const . restoreM

instance MonadIO m => MonadLoggerJSON (LoggingJSONT m) where
    monadLoggerJSONLog a b c d = LoggingJSONT $ \f -> liftIO $ f a b (toLogStr c) d

instance MonadUnliftIO m => MonadUnliftIO (LoggingJSONT m) where
    askUnliftIO = LoggingJSONT $ \f -> withUnliftIO $ \u ->
        pure (UnliftIO (unliftIO u . flip runLoggingJSONT f))

instance MonadLoggerJSON m => MonadLoggerJSON (MaybeT m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (ExceptT e m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (IdentityT m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (StateT s m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance (MonadLoggerJSON m, Monoid w) => MonadLoggerJSON (WriterT w m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (ContT r m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (ReaderT r m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance (MonadLoggerJSON m, Monoid w) => MonadLoggerJSON (RWST r w s m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance MonadLoggerJSON m => MonadLoggerJSON (Strict.StateT s m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance (MonadLoggerJSON m, Monoid w) => MonadLoggerJSON (Strict.WriterT w m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta
instance (MonadLoggerJSON m, Monoid w) => MonadLoggerJSON (Strict.RWST r w s m) where
    monadLoggerJSONLog loc lvl msg meta = Trans.lift $ monadLoggerJSONLog loc lvl msg meta


runStdoutLoggingJSONT ::
       LogLevel -- ^ Minimum level at which messages are logged
    -> Maybe SourceVersion -- ^ Source version, e.g. Git commit SHA
    -> LoggingJSONT m a -- ^ Logging action
    -> m a
runStdoutLoggingJSONT minLevel sourceVersion =
    (`runLoggingJSONT` defaultOutput sourceVersion stdout) . filterLogger (\level _ -> level >= minLevel)

runStdoutLoggingJSONTVerbose ::
       LoggingJSONT m a -- ^ Logging action
    -> m a
runStdoutLoggingJSONTVerbose = runStdoutLoggingJSONT LevelDebug Nothing

filterLogger :: (LogLevel -> [Pair] -> Bool)
             -> LoggingJSONT m a
             -> LoggingJSONT m a
filterLogger p (LoggingJSONT f) = LoggingJSONT $ \logger ->
    f $ \loc level msg meta ->
        when (p level meta) $ logger loc level msg meta

defaultOutput :: Maybe SourceVersion -> Handle -> Loc -> LogLevel -> LogStr -> [Pair] -> IO ()
defaultOutput sourceVersion handle' loc level msg meta = do
    time <- getCurrentTime
    TIO.hPutStrLn handle' (defaultFormat sourceVersion time loc level msg meta)

defaultFormat :: Maybe SourceVersion -> UTCTime -> Loc -> LogLevel -> LogStr -> [Pair] -> Text
defaultFormat sourceVersion time loc level msg meta =
    encodeJSON $
    object $
    [ "time" .= time
    , "type" .= ("app" :: Text)
    , "level" .= formatLogLevel level
    , "message" .= logStrToText msg
    , "location" .= toJSON location
    , "sourceVersion" .= toJSON sourceVersion
    ] <>
    case meta of
        [] -> []
        _ -> ["meta" .= object meta]

  where
    location :: Maybe Value
    location
        | isDefaultLoc loc = Nothing
        | otherwise = Just $ fileLocation loc

encodeJSON :: (ToJSON a) => a -> Text
encodeJSON = lenientDecodeUtf8 . BL.toStrict . encodePretty' prettyEncodeConfig

-- Copy of private function:
-- http://haddock.stackage.org/lts-10.10/monad-logger-0.3.28.2/src/Control.Monad.Logger.html#isDefaultLoc
isDefaultLoc :: Loc -> Bool
isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)) = True
isDefaultLoc _ = False

-- | Taken from file-location package turn the TH Loc loaction information
-- into a human readable string leaving out the loc_end parameter.
fileLocation :: Loc -> Value
fileLocation Loc {..} =
    object
        [ "package" .= loc_package
        , "module" .= T.pack loc_module
        , "file" .= T.pack (loc_filename <> ":" <> show line <> ":" <> show char)
        ]
  where
    (line, char) = loc_start

formatLogLevel :: LogLevel -> Text
formatLogLevel LevelDebug = "debug"
formatLogLevel LevelInfo = "info"
formatLogLevel LevelWarn = "warn"
formatLogLevel LevelError = "error"
formatLogLevel (LevelOther txt) = txt

-- | Convert 'LogStr' to 'Text'.
logStrToText :: LogStr -> Text
logStrToText = lenientDecodeUtf8 . fromLogStr

-- | Safely decode UTF-8 @ByteString@s.
lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = T.decodeUtf8With T.lenientDecode

-- JSON
prettyEncodeConfig :: Config
prettyEncodeConfig = defConfig {confIndent = Spaces 0, confCompare = keyCompare}

keyCompare :: Text -> Text -> Ordering
keyCompare = keyOrder keyOrdering <> compare

keyOrdering :: [Text]
keyOrdering = ["time", "type", "level", "message", "meta", "location"]

---
---
---

logDebug :: Q Exp
logDebug = logJSON LevelDebug

logInfo :: Q Exp
logInfo = logJSON LevelInfo

logWarn :: Q Exp
logWarn = logJSON LevelWarn

logError :: Q Exp
logError = logJSON LevelError

logOther :: Text -> Q Exp
logOther = logJSON . LevelOther

---
---
---

logDebug_ :: Q Exp
logDebug_ = logJSON_ LevelDebug

logInfo_ :: Q Exp
logInfo_ = logJSON_ LevelInfo

logWarn_ :: Q Exp
logWarn_ = logJSON_ LevelWarn

logError_ :: Q Exp
logError_ = logJSON_ LevelError

logOther_ :: Text -> Q Exp
logOther_ = logJSON_ . LevelOther

---
---
---

logJSON :: LogLevel -> Q Exp
logJSON level = [|monadLoggerJSONLog $(qLocation >>= liftLoc) $(lift level)|]

logJSON_ :: LogLevel -> Q Exp
logJSON_ level = [|\msg -> monadLoggerJSONLog $(qLocation >>= liftLoc) $(lift level) msg []|]

-- |
tShow :: Show a => a -> Text
tShow = pack . show
