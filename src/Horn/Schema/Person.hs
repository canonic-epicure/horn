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

query = all Person `restrict` #id == 11


Person :: Proxy Person

p = Person.tableReference

query = (select all from Person).restrict #name == "someName"

query = (select distinct from Person).restrict #name == "someName"

query = 
    `select`
        `distinct`
        [ p#firstName, #lastName, #passwordHash, o#name ] 
    `from`
        (Person `as` "p" `leftJoin` Organization `on` p#orgId = o#id)
    `where`
        (p#name == "someName" && o#someIntAttr < `$1`).

    where
        p = Person
        


instance Show Query where
    show query = undefined


doSomething :: query -> IO Either QueryError [ QueryResult ]

doQueryAsList = do
    Right result@(head:rest) <- doQueryAsList query

    let
        sum = head #name + head #another

        (head `as` Person) -- should typecheck

        (head `as` Person) #name + (head `as` Person) #another

    in
        





-- pipes? conduit? does not matter
doQueryAsStream :: query -> Stream Person




