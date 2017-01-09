{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module SillyMe.Store.Repo where

import           Control.Monad.IO.Class
import           SillyMe.Store
import           SillyMe.Store.Engine
import           SillyMe.Store.Model

class Model model => Repo engine model where
  getAll :: MonadIO mio => engine -> mio [UniqueData model]
  getById :: MonadIO mio => engine -> IdType model -> mio (Maybe (UniqueData model))
  save :: MonadIO mio => engine -> UniqueData model -> mio (UniqueData model)
