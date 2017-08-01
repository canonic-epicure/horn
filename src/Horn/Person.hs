module Horn.Person(
    Person
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


-- mathematically "pure", description, but easily convertible to the pre-defined `ResultStream`
-- Proxy as a connection between the values world and types world

-- Just parse SQL and generate this definition from it!!??

-- use TH to generate from the DB catalog data

class (FieldSet self => Table "Person") where
    definition :: self (self-creation descripted in "FieldSet" primitives and serialized as a Haskell source file)

    definition self = do
        empty           = pure Empty
        withFields      = addFieldSet [ Field "type=bigint" "name=firstName" nullable=Bool ] empty
        withRelation1   = addRelation withFields "constrainName1" [ Relation toTable=Company localFields=Vector referenceFields=Vector ]
        withRelation2   = addRelation withRelation1 "constrainName2" [ Relation toTable=Company localFields=Vector referenceFields=Vector ]

    -- or "definition" can be taken from the DB schema

    blackList :: BlackList

    blackList = BlackList [ fields "name1", fields "name2" | /regexp/ ]

    whiteList :: WhiteList

    whiteList = WhiteList [ "name" | /regexp/ ]
