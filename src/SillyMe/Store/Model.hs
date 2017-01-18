module SillyMe.Store.Model where

import           Data.Text
import           Data.UUID
import           GHC.Generics
import           TOSPIO.Database.Garbage.Model
import           TOSPIO.Database.Garbage.SQLite.TH


data Lang = Lang { langName :: Text
                 } deriving (Generic)

instance Model Lang

mkSQLiteModel MkSQLiteModelParams ''Lang

data SillyCategory = SillyCategory { catName :: Text
                                   } deriving (Generic)

instance Model SillyCategory


data CodeSnippet = CodeSnippet { code :: Text
                               } deriving (Generic)
