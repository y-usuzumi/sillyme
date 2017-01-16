module SillyMe.Store.Repo where

import           Control.Monad.IO.Class
import           Data.Proxy
import           SillyMe.Store
import           SillyMe.Store.Engine
import           SillyMe.Store.Model

class Model model => Repo engine model where
  init :: MonadIO mio => engine -> Proxy model -> mio ()
  getAll :: MonadIO mio => engine -> mio [UniqueData model]
  getById :: MonadIO mio => engine -> IdType model -> mio (Maybe (UniqueData model))
  save :: MonadIO mio => engine -> UniqueData model -> mio (UniqueData model)
