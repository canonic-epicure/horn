module Cognitum.Person(
    Person
) where

-- generated modules
import Cognitum.Schema.PersonTable

-- type-level data (facts)
-------------------------

-----
-- mapping from SQL type to Haskell Type - will be used in the record definition
-----
data family SqlTypeMapping name 

data instance SqlTypeMapping "VARCHAR" Text encoder decoder


----------
-- list of databases
----------

data family Database name db

data instance Schema "information_schema"


----------
-- list of schemas
----------
data family Schema name db

data instance Schema "information_schema"


----------
-- tables
----------

data family Column name sqlType nullable hasDefault box


data family Table name [ columns ] schema box

data instance SqlTable "person" [
    SqlColumn "id"
    SqlColumn "name"
    SqlColumn "birthday"
]


type family SchemaHasTable schema tableName :: columnName
    SchemaHasTable "information_schema" "id" = SqlColumn "id"


----------
-- columns
----------


type family TableHasColumn  table columnName :: columnName
    TableHasColumn "Person" "id" = SqlColumn "id"


----------
-- primary keys
----------

type family TableHasPrimaryKey  table columnName :: columnName
    TableHasColumn "Person" "id" = SqlColumn "id"

----------
-- foreign keys
----------

type family TableHasForeignKey  table columnName :: columnName
    TableHasColumn "Person" "id" = SqlColumn "id"


----------
-- corresponding record definition
----------

data PersonRecord box = PersonRecord {
    id      :: GetColumnOf "Person" (SqlTypeMapping GetTypeOf "id") -- the actual type of the column (with "box" field set to haskell type)
    name    :: GetColumnOf "Person"
}

-- of course it should use "micro-lenses"
$(mkLenses PersonRecord)