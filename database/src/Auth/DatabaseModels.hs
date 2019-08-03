{-# LANGUAGE NoDeriveAnyClass, GeneralizedNewtypeDeriving, QuasiQuotes #-}

module Auth.DatabaseModels where

import           Protolude
import           Database.Persist.Postgresql.JSON ()
import           Database.Persist.TH              (mkDeleteCascade, mkPersist, persistLowerCase, share, sqlSettings)
import           Data.Text                        (Text)

share [mkPersist sqlSettings, mkDeleteCascade sqlSettings] [persistLowerCase|
DbUser json sql=users
    name           Text
    email          Text
    hashedPassword Text
    DbUserUniqueEmail email
    DbUserLogin email hashedPassword
    deriving Show Eq
|]
