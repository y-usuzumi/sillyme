{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module SillyMe.Store.Repo.SQLite.Generics where

import           Data.HashMap.Strict as HM
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable
import           Data.UUID           (UUID)
import           GHC.Generics

type SQLTypeMap = HM.HashMap TypeRep String

-----------------
-- SQLTypeable --
-----------------

class SQLTypeable t where
  sqlType :: Proxy t -> String

instance SQLTypeable Int where
  sqlType _ = "INTEGER"

instance SQLTypeable Integer where
  sqlType _ = "INTEGER"

instance SQLTypeable String where
  sqlType _ = "TEXT"

instance SQLTypeable UUID where
  sqlType _ = "VARCHAR(40)"

class GQueryPack f where
  gfields :: f a -> [(Text, Text)]

instance GQueryPack U1 where
  gfields U1 = []

instance (GQueryPack l, GQueryPack r) => GQueryPack (l :*: r) where
  gfields (l :*: r) = gfields l ++ gfields r

instance {-# Overlappable #-} GQueryPack f => GQueryPack (M1 i c f) where
  gfields (M1 m) = gfields m

instance {-# Overlapping #-} (GQueryPack f, Selector c) => GQueryPack (M1 S c (f :: * -> *)) where
  gfields (M1 m) = let
    n = selName (undefined :: i c f p)
    in [(T.pack n, snd $ head $ gfields m)]

instance SQLTypeable c => GQueryPack (K1 i c) where
  gfields _ = [(undefined, T.pack $ sqlType (Proxy :: Proxy c))]

class QueryPack q where
  fields  :: q -> [(Text, Text)]
  default fields :: (Generic q, GQueryPack (Rep q)) => q -> [(Text, Text)]
  fields a = gfields (from a)

instance QueryPack Int where
  fields _ = []

instance QueryPack String where
  fields _ = []

instance QueryPack Text where
  fields _ = []

instance QueryPack UUID where
  fields _ = []
