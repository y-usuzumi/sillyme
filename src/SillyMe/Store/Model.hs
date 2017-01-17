module SillyMe.Store.Model where

import           Data.Text
import           Data.UUID
import           GHC.Generics
import           SillyMe.Store.Repo.SQLite.TH

class Model model where
  type IdType model :: *
  type IdType model = UUID

type UniqueData model = (IdType model, model)


------------
-- Models --
------------

data Lang = Lang { langId :: UUID
                 , langName :: Text
                 } deriving (Generic)

instance Model Lang

mkSQLiteModel MkSQLiteModelParams{ firstFieldAsPK = True } ''Lang

data SillyCategory = SillyCategory { catName :: Text
                                   } deriving (Generic)

instance Model SillyCategory


data CodeSnippet = CodeSnippet { code :: Text
                               } deriving (Generic)
