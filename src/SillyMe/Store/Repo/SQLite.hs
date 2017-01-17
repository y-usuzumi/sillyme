module SillyMe.Store.Repo.SQLite where

import           Control.Arrow
import           Control.Monad.IO.Class
import           Data.String.Interpolate
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.UUID
import           Data.UUID.V4
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField
import {-# SOURCE#-}          SillyMe.Store.Engine.SQLite
import           SillyMe.Store.Model
import           SillyMe.Store.Repo
import           SillyMe.Store.Repo.SQLite.TH

instance FromField UUID where
  fromField f@(Field (SQLText t) _)
    | Just uuid <- fromString $ T.unpack t = Ok uuid
    | otherwise = returnError ConversionFailed f "Invalid text field"
  fromField f                     = returnError ConversionFailed f "expecting SQLText column type"

instance ToField UUID where
  toField = SQLText . T.pack . toString

----------
-- LANG --
----------

createLangTableQuery :: Query
createLangTableQuery = Query $ T.pack [i|
create table lang (
id varchar(40) primary key,
name text
)
|]

getAllLangsQuery :: Query
getAllLangsQuery = "select id, name from lang"

getLangByIdQuery :: Query
getLangByIdQuery = "select id, name from lang where id=:id"

createLangQuery :: Query
createLangQuery = "insert into lang (id, name) values (:id, :name)"

updateLangQuery :: Query
updateLangQuery = "update lang set name=:name where id=:id"

getLangIdByRowIdQuery :: Query
getLangIdByRowIdQuery = "select id from lang where ROWID=:rowid"

instance Repo SQLiteEngine Lang where
  init SQLiteEngine{..} _ = liftIO $ do
    withConnection location $ \conn -> do
      executeNamed conn createLangTableQuery []

  getAll SQLiteEngine{..} = liftIO $ do
    withConnection location $ \conn -> do
      r <- query_ conn getAllLangsQuery :: IO [(UUID, Text)]
      return $ map (id *** (\ name -> Lang { langId = nil, langName = name })) r

  getById SQLiteEngine{..} uuid = liftIO $ do
    withConnection location $ \conn -> do
      r <- queryNamed conn getLangByIdQuery [ ":id" := uuid ]
      case r of
        []             -> return Nothing
        (uuid, lang):_ -> return $ Just $ (uuid, Lang { langName = lang })

  save SQLiteEngine{..} (uuid, lang@Lang{..}) = liftIO $ do
    withConnection location $ \conn -> do
      if uuid == nil
        then do
        executeNamed conn createLangQuery [ ":id" := uuid, ":name" := langName ]
        lastRowId <- lastInsertRowId conn
        r <- queryNamed conn getLangIdByRowIdQuery [ ":rowid" := lastRowId ]
        case r of
          []           -> return (nil, lang)
          Only uuid':_ -> return (uuid', lang)
        else do
        executeNamed conn createLangQuery [ ":id" := uuid, ":name" := langName ]
        return (uuid, lang)

-------------------
-- SillyCategory --
-------------------

createSillyCategoryTableQuery :: Query
createSillyCategoryTableQuery = Query $ T.pack [i|
create table silly_category (
id varchar(40) primary key,
name text
)
|]

getAllSillyCategoriesQuery :: Query
getAllSillyCategoriesQuery = "select id, name from silly_category"

getSillyCategoryByIdQuery :: Query
getSillyCategoryByIdQuery = "select id, name from silly_category where id=:id"

createSillyCategory :: Query
createSillyCategory = "insert into silly_category (id, name) values (:id, :name)"

updateSillyCategory :: Query
updateSillyCategory = "update silly_category set name=:name where id=:id"

getSillyCategoryIdByRowIdQuery :: Query
getSillyCategoryIdByRowIdQuery = "select id from silly_category where ROWID=:rowid"
