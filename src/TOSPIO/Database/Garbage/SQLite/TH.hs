module TOSPIO.Database.Garbage.SQLite.TH where

import           Data.Char
import qualified Data.HashMap.Strict               as HM
import           Data.List
import           Data.Proxy
import           Data.String.Interpolate
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Typeable
import           Data.UUID                         (UUID)
import           Database.SQLite.Simple
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax
import           TOSPIO.Database.Garbage.Model
import           TOSPIO.Database.Garbage.QueryPack

sqlTypeMap :: HM.HashMap TypeRep String
sqlTypeMap = HM.fromList [ (typeOf (undefined :: Int), "INTEGER")
                         , (typeOf (undefined :: Integer), "INTEGER")
                         , (typeOf (undefined :: String), "TEXT")
                         , (typeOf (undefined :: Text), "TEXT")
                         , (typeOf (undefined :: UUID), "VARCHAR(40)")
                         ]


typeOfExp :: Type -> ExpQ
typeOfExp t = appE
  (varE (mkName "typeOf"))
  (sigE (varE $ mkName "undefined") (return t))


data MkSQLiteModelParams = MkSQLiteModelParams

mkSQLiteModel :: MkSQLiteModelParams -> Name -> Q [Dec]
mkSQLiteModel MkSQLiteModelParams name = do
  TyConI dec <- reify name
  let fields = case dec of
        DataD _ _ _ _ [] _      -> error "Goddamn"
        DataD _ _ _ _ (con:_) _ -> mkFieldExprs con
        otherwise               -> error "Goddamn"
      tableName = camelToLCWU $ nameBase name
  [d|instance QueryPack $(conT name) where
       fields _ = $(listE fields)
       tableName _ = tableName|]
  where
    camelToLCWU "" = ""
    camelToLCWU (c:cs) = toLower c:camelToLCWU_ cs
      where
        camelToLCWU_ "" = ""
        camelToLCWU_ (c:cs)
          | isUpper c = '_':toLower c:camelToLCWU_ cs
          | otherwise = c:camelToLCWU_ cs
    mkFieldExprs :: Con -> [ExpQ]
    mkFieldExprs (RecC name [])    = error "Goddamn"
    mkFieldExprs (RecC name o) =
      mkPKFieldExpr [t|IdType $(conT name)|] : map mkFieldExpr o
    mkFieldExpr :: (Name, Bang, Type) -> Q Exp
    mkFieldExpr (name, _, type_) =
      [|FieldDef { name = nameBase name
                 , type_ = typeOf (undefined :: $(return type_))
                 , sqlType = sqlTypeMap HM.! (typeOf (undefined :: $(return type_)))
                 , isPK = False
                 }|]
    mkPKFieldExpr :: Q Type -> Q Exp
    mkPKFieldExpr type_ =
      [|FieldDef { name = "id"
                 , type_ = typeOf (undefined :: $(type_))
                 , sqlType = sqlTypeMap HM.! (typeOf (undefined :: $(type_)))
                 , isPK = True
                 }|]

