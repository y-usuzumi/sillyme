{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module SillyMe.Store.Repo.SQLite where

import           Control.Arrow
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
import {-# SOURCE #-}          SillyMe.Store.Engine.SQLite
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

createLang :: Query
createLang = "insert into lang (id, name) values (:id, :name)"

updateLang :: Query
updateLang = "update lang set name=:name where id=:id"

getLangIdByRowId :: Query
getLangIdByRowId = "select id from lang where ROWID=:rowid"

instance Repo SQLiteEngine Lang where
  init SQLiteEngine{..} = undefined
  getAll SQLiteEngine{..} = liftIO $ do
    withConnection location $ \conn -> do
      r <- query_ conn getAllLangsQuery :: IO [(UUID, Text)]
      return $ map (id *** Lang) r

  getById SQLiteEngine{..} uuid = liftIO $ do
    withConnection location $ \conn -> do
      r <- queryNamed conn getLangByIdQuery [ ":id" := uuid ]
      case r of
        [] -> return Nothing
        (uuid, lang):_ -> return $ Just $ (uuid, Lang { langName = lang })

  save SQLiteEngine{..} (uuid, lang@Lang{..}) = liftIO $ do
    withConnection location $ \conn -> do
      if uuid == nil
        then do
        executeNamed conn createLang [ ":id" := uuid, ":name" := langName ]
        lastRowId <- lastInsertRowId conn
        r <- queryNamed conn getLangIdByRowId [ ":rowid" := lastRowId ]
        case r of
          [] -> return (nil, lang)
          Only uuid':_ -> return (uuid', lang)
        else do
        executeNamed conn createLang [ ":id" := uuid, ":name" := langName ]
        return (uuid, lang)
