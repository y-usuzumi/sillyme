module TOSPIO.Database.Garbage.QueryPack where

import           Data.List
import           Data.Proxy
import           Data.String.Interpolate
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Typeable
import           Database.SQLite.Simple
import           TOSPIO.Database.Garbage.Model


data FieldDef = FieldDef { name    :: String
                         , type_   :: TypeRep
                         , sqlType :: String
                         , isPK    :: Bool
                         } deriving Show

class Model model => QueryPack model where
  tableName :: Proxy model -> String
  fields :: Proxy model -> [FieldDef]

  createTableQuery :: Proxy model -> Query
  createTableQuery proxy = Query $ T.pack [i|create table #{tableName proxy} (
  #{intercalate ",\n" $ map renderField (fields proxy)}
)|]
    where
      renderField FieldDef{..} = [i|#{name} #{sqlType} #{if isPK then "primary key" else ""}|]

  getAllQuery :: Proxy model -> Query
  getAllQuery proxy = Query $ T.pack [i| |]
