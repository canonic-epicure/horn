module Horn.Database(
    Database,
    describeDatabase
) where

import              Record
import              Record.Lens

import              Data.Text
import              Data.Vector
import qualified    Data.Map.Strict as DMS

import qualified    Hasql.Connection as HC
import qualified    Hasql.Query as HQ
import qualified    Hasql.Session as HS
import qualified    Hasql.Encoders as HE
import qualified    Hasql.Decoders as HD

------------------------------------------------------------------------------------------------------------------------
newtype Database = Database [r| {
    name        :: Text,
    schemas     :: DMS.Map Text Schema
} |]


------------------------------------------------------------------------------------------------------------------------
newtype Schema = Schema [r| {
    name        :: Text,
    tables      :: DMS.Map Text Table
} |]


--------------------------------------------------------------------------------------------------------------------------
newtype Table = Table [r| {
    name        :: Text,
    fields      :: DMS.Map Text Field,
    primaryKeys :: Vector PrimaryKey,
    foreignKeys :: Vector ForeignKey
} |]


class Entity name where
    getName :: Text

    runQuery :: Query -> ResultSet

--------------------------------------------------------------------------------------------------------------------------
newtype Field = Field [r| {
    name        :: Text,
    typeStr     :: Text,
    nullable    :: Bool
} |]


------------------------------------------------------------------------------------------------------------------------
newtype PrimaryKey = PrimaryKey [r| {
    name        :: Text,
    fields      :: Vector Field
} |]

------------------------------------------------------------------------------------------------------------------------
newtype ForeignKey = ForeignKey [r| {
    name                :: Text,
    fields              :: Vector Field,
    referenceTable      :: Table,
    referenceFields     :: Vector Field
} |]

------------------------------------------------------------------------------------------------------------------------
describeDatabase :: HC.Connection -> IO (Either HS.Error Database)

describeDatabase connection = do
    queryRes <- HS.run (HS.query () selectSchemasQuery) connection

    case queryRes of
        Left error ->
            return $ Left error
        Right res -> do
            return $ Right (Database [r| {
                name    = "SomeDB",
                schemas = DMS.empty
            } |])
    where
        selectSchemasQuery :: HQ.Query () [ Text ]

        selectSchemasQuery =
            HQ.statement sql encoder decoder True
            where
                sql         = "SELECT schema_name FROM information_schema.schemata as s where schema_name NOT LIKE 'pg_t%'"

                encoder     = HE.unit
                decoder     = HD.rowsList (HD.value HD.text)


------------------------------------------------------------------------------------------------------------------------
doSomething :: HC.Connection -> IO (Either HS.Error [ User ])

doSomething = do
    result <- [sql|
        select
            *
        from
            user as u
        left join
            applicaiton as a on u.id = a.userId
        where
            u.name like 'asd'
    |]

    queryUser