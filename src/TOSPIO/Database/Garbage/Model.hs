module TOSPIO.Database.Garbage.Model where

import           Data.Text
import           Data.UUID
import           GHC.Generics

class Model model where
  type IdType model :: *
  type IdType model = UUID

type UniqueData model = (IdType model, model)
