module Horn.Field(
    Field,
--    getDbType
) where

import qualified    Horn.DbType as HD

----import              Record
----import              Record.Lens


--import              GHC.TypeLits
import              Data.Text
--import              Data.Proxy


--
--import              Data.Text
----import              Data.Vector
----import qualified    Data.Map.Strict as DMS
----
----import qualified    Hasql.Connection as HC
----import qualified    Hasql.Query as HQ
----import qualified    Hasql.Session as HS
----import qualified    Hasql.Encoders as HE
----import qualified    Hasql.Decoders as HD
--


data Field = Field {
    name        : Text,
    dbType      : HD.DbType,
    isNullable  : Bool
}

--data Field =
--    forall name . (KnownSymbol name) =>
--    Field {
--        name        :: Proxy name,
--        dbType      :: HD.DbType
--    }

--class KnownSymbol self => DbTypeClass self where
--    getDbType :: Proxy self -> Text
--
--    getDbType self = pack $ symbolVal self
--
--
--
----class KnownSymbol name, HD.DbType dbType => Field name dbType where
----    getName :: Text
----    getDbType :: self -> Text
------    isNullable :: Bool
----
----
