{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module SillyMe.Store.Repo.SQLite.TH where

import           Data.Proxy
import           Data.Typeable               (TypeRep)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

data FieldDef = FieldDef { name    :: String
                         , type_   :: TypeRep
                         , sqlType :: String
                         , isPK    :: Bool
                         } deriving (Lift, Show)

class QueryPack model where
  tableName :: Proxy model -> String
  fields :: Proxy model -> [FieldDef]

typeOfExp :: Type -> Exp
typeOfExp t = appE
  (varE (mkName "typeOf"))
  (sigE (mkName "undefined") t)

mkSQLiteModel :: Name -> Q [Dec]
mkSQLiteModel name = do
  TyConI dec <- reify name
  let fields = case dec of
        DataD _ _ _ _ [] _      -> error "Goddamn"
        DataD _ _ _ _ (con:_) _ -> mkFields con
        otherwise               -> error "Goddamn"
  runIO $ do
    putStrLn "Fields:"
    print fields
  [d|instance QueryPack $(conT name) where
       fields _ = fields|]
  where
    mkFields :: Con -> [FieldDef]
    mkFields (RecC name [])    = error "Goddamn"
    mkFields (RecC name (f:o)) = mkPKField f : map mkNonPKField o
    mkFields _                 = error "Goddamn"
    mkField :: Bool -> (Name, Bang, Type) -> FieldDef
    mkField isPK (name, _, type_) = let
      in
      FieldDef { name = nameBase name
               , type_ = $(typeOfExp type_)
               , sqlType = show type_
               , isPK = isPK
               }
    mkPKField = mkField True
    mkNonPKField = mkField False

