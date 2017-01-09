{-# LANGUAGE TypeFamilies #-}

module SillyMe.Store.Model where

import           Data.Text
import           Data.UUID

class Model model where
  type IdType model :: *
  type IdType model = UUID

type UniqueData model = (IdType model, model)


------------
-- Models --
------------

data Lang = Lang { langName :: String
                 }

instance Model Lang


data SillyCategory = SillyCategory { catName :: String
                                   }

instance Model SillyCategory
