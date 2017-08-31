module Horn.Sql(
    Sql
) where

--import              Record
--import              Record.Lens

--import              Data.Text
--import              Data.Vector
--import qualified    Data.Map.Strict as DMS
--
--import qualified    Hasql.Connection as HC
--import qualified    Hasql.Query as HQ
--import qualified    Hasql.Session as HS
--import qualified    Hasql.Encoders as HE
--import qualified    Hasql.Decoders as HD

------------------------------------------------------------------------------------------------------------------------
import qualified    Horn.Something as HS
import qualified    Horn.Operators(.==, .<)

import qualified    YourApp.DbSchema as DB


-- Person has many BlogPost

type Person     = HS.Entity "Person"

type Person     = Db Table "Person"
type P          = Person

someQuery :: ValidQuery

data ValidQuery where
    ValidQuery = SqlExpression


select :: ColumnsList -> RelationClause -> RelationClause | SqlExpression

from ::

someQuery =
    (select [ "P"."id", P."name", ])
    `from`
        Person as "P"
    `where`
        (P."id" == (??) && Person.name `like` 'asd')
    `orderBy` (HS.desc Person.id)
