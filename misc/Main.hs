--{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators, OverloadedStrings #-}

module Main where

import Data.Proxy
import Data.Kind

import Data.Singletons.TH
import Data.Singletons.Prelude.List


----------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = print "yo"

$(singletons [d|

    data Token = SelectKeyword | FromKeyword | WhereKeyword | StatementStart | StatementEnd deriving Show

    -- data Control = StatementStart | StatementEnd deriving Show

    tokenCanBeConsumed :: Token -> Token -> Bool

    tokenCanBeConsumed StatementStart StatementStart = True
    tokenCanBeConsumed StatementStart SelectKeyword = True
    tokenCanBeConsumed StatementStart FromKeyword = False
    tokenCanBeConsumed StatementStart WhereKeyword = False

    tokenCanBeConsumed SelectKeyword StatementStart = True
    tokenCanBeConsumed SelectKeyword SelectKeyword = False
    tokenCanBeConsumed SelectKeyword FromKeyword = True
    tokenCanBeConsumed SelectKeyword WhereKeyword = False


    tokenCanBeConsumed2 :: [ Token ] -> Token -> Bool

    tokenCanBeConsumed2 prev@[] StatementStart = True
    tokenCanBeConsumed2 prev@[] SelectKeyword = False
    tokenCanBeConsumed2 prev@[] FromKeyword = False
    tokenCanBeConsumed2 prev@[] WhereKeyword = False
    tokenCanBeConsumed2 prev@[] StatementEnd = False

    tokenCanBeConsumed2 prev@(StatementStart : []) StatementStart = False
    tokenCanBeConsumed2 prev@(StatementStart : []) SelectKeyword = True
    tokenCanBeConsumed2 prev@(StatementStart : []) FromKeyword = False
    tokenCanBeConsumed2 prev@(StatementStart : []) WhereKeyword = False

    tokenCanBeConsumed2 prev@(SelectKeyword : [ StatementStart ]) StatementStart = False
    tokenCanBeConsumed2 prev@(SelectKeyword : [ StatementStart ]) SelectKeyword = False
    tokenCanBeConsumed2 prev@(SelectKeyword : [ StatementStart ]) FromKeyword = True
    tokenCanBeConsumed2 prev@(SelectKeyword : [ StatementStart ]) WhereKeyword = False
    
    |])

----------------------------------------------------------------------------------------------------------------------------
-- literal :: String -> Token

-- literal name = Literal name

----------------------------------------------------------------------------------------------------------------------------
start :: Sing 'StatementStart

start = SStatementStart

----------------------------------------------------------------------------------------------------------------------------
end :: Sing 'StatementEnd

end = SStatementEnd

----------------------------------------------------------------------------------------------------------------------------
select :: Sing 'SelectKeyword

select = SSelectKeyword

----------------------------------------------------------------------------------------------------------------------------
where' :: Sing 'WhereKeyword

where' = SWhereKeyword

----------------------------------------------------------------------------------------------------------------------------
from :: Sing 'FromKeyword

from = SFromKeyword

data TokenConsumptionAccumulator = TokenConsumptionAccumulator [ Token ] deriving Show

-- Sing (tokenType :: Token) -> Sing (LegalFollowingToken tokenType) -> Sing (LegalFollowingToken (LegalFollowingToken tokenType)) -> TokenConsumptionAccumulator

-- (Sing StatementStart -> Sing (LegalFollowingToken tokenType) -> Sing (LegalFollowingToken (LegalFollowingToken tokenType)) -> TokenConsumptionAccumulator


type ConsumerAsAFunction consumer tokenType = 
    Sing (tokenType :: Token) -> consumer

----------------------------------------------------------------------------------------------------------------------------
class TokenConsumer consumer (previouslyConsumedTokens :: [ Token ]) where
    consumeSqlTokens' :: TokenConsumptionAccumulator -> consumer

instance
    TokenConsumer TokenConsumptionAccumulator previouslyConsumedTokens
    where
        consumeSqlTokens' (TokenConsumptionAccumulator tokens) = TokenConsumptionAccumulator (reverse tokens)

instance {-# OVERLAPPABLE #-}
    (
        TokenConsumer consumer1 previouslyConsumedTokens,    
        --TokenConsumer (ConsumerAsAFunction consumer1 lastConsumedTokenType previouslyConsumedTokens),
        TokenCanBeConsumed2 previouslyConsumedTokens tokenType ~ 'True
        -- ,
        -- previouslyConsumedTokens2 ~ (tokenType : previouslyConsumedTokens)
    )
    =>
    TokenConsumer (ConsumerAsAFunction consumer2 tokenType) (tokenType : previouslyConsumedTokens)
    where
        consumeSqlTokens' (TokenConsumptionAccumulator tokens) =
            \sToken -> consumeSqlTokens' (TokenConsumptionAccumulator (fromSing sToken : tokens))

instance {-# OVERLAPPING #-}
    (
        TokenCanBeConsumed2 '[] tokenType ~ 'True
    )
    =>
    TokenConsumer (ConsumerAsAFunction consumer tokenType) '[]
    where
        consumeSqlTokens' (TokenConsumptionAccumulator tokens) =
            \sToken -> consumeSqlTokens' (TokenConsumptionAccumulator (fromSing sToken : tokens))

consumeSqlToken :: (TokenConsumer consumer p) => consumer

consumeSqlToken = consumeSqlTokens' (TokenConsumptionAccumulator [])


----------------------------------------------------------------------------------------------------------------------------
validQuery :: TokenConsumptionAccumulator

-- -- validQuery = consumeSqlToken SStatementStart where'
validQuery = (consumeSqlToken :: (TokenConsumer consumer [ SelectKeyword, StatementStart ]) => consumer) start select

----------------------------------------------------------------------------------------------------------------------------
-- inValidQuery :: [ Token ]

-- inValidQuery = (consumeSqlToken :: (ConsumerAsAFunction consumer StatementStart)) select where'


--query = consumeSqlToken select (literal "c") from (literal "t") where'

----------------------------------------------------------------------------------------------------------------------------
-- optional "all"
--query = select all (literal "c") from (literal "t") where ("t" = "z")
