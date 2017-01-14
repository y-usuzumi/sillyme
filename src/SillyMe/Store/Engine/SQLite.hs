{-# LANGUAGE RecordWildCards #-}

module SillyMe.Store.Engine.SQLite where

import           Data.Proxy
import           SillyMe.Store.Engine
import           SillyMe.Store.Model
import           SillyMe.Store.Repo        as R
import           SillyMe.Store.Repo.SQLite as RSQLite

data SQLiteEngine = SQLiteEngine { location :: FilePath
                                 }

instance Engine SQLiteEngine where
  init ng = sequence_ [ R.init ng (Proxy :: Proxy Lang)
                      ]
