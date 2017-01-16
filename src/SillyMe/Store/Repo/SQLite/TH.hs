module SillyMe.Store.Repo.SQLite.TH where

import           Data.Proxy
import           Data.Typeable
import           Data.Text (Text)
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax
import qualified Data.HashMap.Strict as HM
import           Data.UUID (UUID)

sqlTypeMap :: HM.HashMap TypeRep String
sqlTypeMap = HM.fromList [ (typeOf (undefined :: Int), "INTEGER")
                         , (typeOf (undefined :: Integer), "INTEGER")
                         , (typeOf (undefined :: String), "TEXT")
                         , (typeOf (undefined :: Text), "TEXT")
                         , (typeOf (undefined :: UUID), "VARCHAR(40)")
                         ]

data FieldDef = FieldDef { name    :: String
                         , type_ :: TypeRep
                         , sqlType :: String
                         , isPK    :: Bool
                         } deriving Show


class QueryPack model where
  tableName :: Proxy model -> String
  fields :: Proxy model -> [FieldDef]

typeOfExp :: Type -> ExpQ
typeOfExp t = appE
  (varE (mkName "typeOf"))
  (sigE (varE $ mkName "undefined") (return t))

mkSQLiteModel :: Name -> Q [Dec]
mkSQLiteModel name = do
  TyConI dec <- reify name
  let fields = case dec of
        DataD _ _ _ _ [] _      -> error "Goddamn"
        DataD _ _ _ _ (con:_) _ -> mkFieldExprs con
        otherwise               -> error "Goddamn"
  runIO $ do
    putStrLn "Fields:"
  [d|instance QueryPack $(conT name) where
       fields _ = $(listE fields)|]
  where
    mkFieldExprs :: Con -> [ExpQ]
    mkFieldExprs (RecC name [])    = error "Goddamn"
    mkFieldExprs (RecC name (f:o)) =
      mkPKFieldExpr f : (map mkNonPKFieldExpr o)
    mkFieldExprs _                 = error "Goddamn"
    mkFieldExpr :: Bool -> (Name, Bang, Type) -> Q Exp
    mkFieldExpr isPK (name, _, type_) = let
      in
      [|FieldDef { name = nameBase name
                 , type_ = typeOf (undefined :: $(return type_))
                 , sqlType = sqlTypeMap HM.! (typeOf (undefined :: $(return type_)))
                 , isPK = isPK
                 }|]
    mkPKFieldExpr = mkFieldExpr True
    mkNonPKFieldExpr = mkFieldExpr False

