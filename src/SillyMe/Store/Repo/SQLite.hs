{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module SillyMe.Store.Repo.FileSystem where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.UUID
import           Data.UUID.V4
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Ok
import           SillyMe.Store.Engine.SQLite
import           SillyMe.Store.Model
import           SillyMe.Store.Repo

instance FromField UUID where
  fromField f@(Field (SQLText t) _)
    | Just uuid <- fromString $ T.unpack t = Ok uuid
    | otherwise = returnError ConversionFailed f "Invalid text field"
  fromField f                     = returnError ConversionFailed f "expecting SQLText column type"

instance ToField UUID where
  toField = SQLText . T.pack . toString

getAllLangsQuery :: Query
getAllLangsQuery = "select id, name from lang"

getLangByIdQuery :: Query
getLangByIdQuery = "select id, name from lang where id=:id"

instance Repo SQLiteEngine Lang where
  getAll SQLiteEngine{..} = liftIO $ do
    conn <- open location
    r <- query_ conn getAllLangsQuery :: IO [(UUID, Text)]
    return (force $ map (id *** Lang) r) >>= seq (close conn) . return

  getById SQLiteEngine{..} uuid = liftIO $ do
    conn <- open location
    r <- queryNamed conn getLangByIdQuery [ ":id" := uuid ]
    case r of
      [] -> return Nothing
      (uuid, lang):_ -> return $ Just $ (uuid, Lang { langName = lang })

  save SQLiteEngine{..} = undefined
