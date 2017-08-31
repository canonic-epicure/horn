module Horn.DbType(
--    DbType(..),
    DbType
--    ,
--    getDbType
--    ,
--    typeBigInt
) where

--import              Record
--import              Record.Lens

import              GHC.TypeLits
import              Data.Text
import              Data.Proxy
--import              Data.Vector
--import qualified    Data.Map.Strict as DMS
--
data DbType =
    forall dbTypeDecl . (KnownSymbol dbTypeDecl, DbTypeClass dbTypeDecl) =>
    DbType (Proxy dbTypeDecl)

class KnownSymbol self => DbTypeClass self where
    getDbType :: Proxy self -> Text

    getDbType self = pack $ symbolVal self


instance DbTypeClass "bigint"

bigInt = DbType (Proxy :: Proxy "bigint")
