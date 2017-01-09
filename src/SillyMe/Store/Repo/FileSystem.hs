{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module SillyMe.Store.Repo.FileSystem where

import           SillyMe.Store.Engine.FileSystem
import           SillyMe.Store.Model
import           SillyMe.Store.Repo

instance Repo FileSystemEngine Lang where
  getAll FileSystemEngine{..} = undefined
  getById FileSystemEngine{..} = undefined
  save FileSystemEngine{..} = undefined
