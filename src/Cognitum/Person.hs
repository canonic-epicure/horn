module Cognitum.Person(
    
) where

-- collection of person-related methods
import Cognitum.Schema.Person



instance TableInterface Person where
    all :: Stream Person

    restrict :: Condition


class (PersonTable self) => Person self where
    getFullName :: self -> Text

    setFullNameFromText :: self -> Text -> self



-----------------------------------------------
-- usage sample
-----------------------------------------------

import qualified Cognitum.Person as Person
import qualified Cognitum.Person as Organization

query = Person.all.restrict #name == "someName"


query = (select all from Person).restrict #name == "someName"

query = (select distinct from Person).restrict #name == "someName"

query = 
    `select`
        `distinct`
        [ p#firstName, #lastName, #passwordHash, o#name ] 
    `from`
        (Person `as` "p" `leftJoin` Organization `on`` p#orgId = o#id)
    `where`
        (p#name == "someName" && o#someIntAttr < `$1`).
        


instance Show Query where
    show query = undefined


doQueryAsList :: query -> IO [ Person ]


doQueryAsStream :: query -> IO [ Person ]

