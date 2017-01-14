{-# LANGUAGE TemplateHaskell #-}

module SillyMe.Store.Repo.SQLite.X where

import Language.Haskell.TH

typeOf :: Type -> Q Exp
typeOf t = [|typeOf (undefined :: $(return t))|]
