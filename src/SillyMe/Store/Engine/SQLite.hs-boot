module SillyMe.Store.Engine.SQLite where

import           Data.Proxy
import           SillyMe.Store.Engine
import           SillyMe.Store.Model

data SQLiteEngine = SQLiteEngine { location :: FilePath
                                 }

instance Engine SQLiteEngine