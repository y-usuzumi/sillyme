module SillyMe.Store.Engine.SQLite where

import SillyMe.Store.Engine

data SQLiteEngine = SQLiteEngine { location :: FilePath
                                 }

instance Engine SQLiteEngine
