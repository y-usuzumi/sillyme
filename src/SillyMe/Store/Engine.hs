module SillyMe.Store.Engine where

import Control.Monad.IO.Class

class Engine e where
  init :: MonadIO mio => e -> mio ()
