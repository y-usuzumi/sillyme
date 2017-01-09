{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module SillyMe.Store.Repo where

import           SillyMe.Store
import           SillyMe.Store.Engine
import           SillyMe.Store.Model

class Model model => Repo engine model where
  getAll :: engine -> [UniqueData model]
  getById :: engine -> IdType model -> UniqueData model

instance Repo FileSystemEngine Lang where
  getAll FileSystemEngine{..} = undefined
  getById FileSystemEngine{..} = undefined
