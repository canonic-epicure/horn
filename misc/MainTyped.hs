{-# OPTIONS_GHC -fprint-explicit-kinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
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


-- data MyType = Var1 | Var2

-- listOfValues :: [ MyType ]

-- listOfValues = [ Var1, Var2 ]


-- listOfTypes :: [ Proxy (a :: MyType) ]

-- listOfTypes = [ Proxy :: Proxy 'Var1, Proxy :: Proxy 'Var2 ]


----------------------------------------------------------------------------------------------------------------------------
main :: IO ()
main = print "yo"


data Token =
  Literal String
  | SelectKeyword
  | FromKeyword
  | WhereKeyword
  | StatementStart
  | StatementEnd

  deriving Show

-- data SelectKeyword
-- data FromKeyword
-- data WhereKeyword
-- data Literal

-- ----------------------------------------------------------------------------------------------------------------------------
-- type family AccumulateVisitedTypes previosulyVisitedTypes newType

-- type instance AccumulateVisitedTypes [] a =
--   [ a ]

-- type instance AccumulateVisitedTypes previous a =
--   a : previous

----------------------------------------------------------------------------------------------------------------------------
type family TokenCanBeConsumed (lastConsumedToken :: Token) (token :: Token) :: Bool where
  TokenCanBeConsumed StatementStart SelectKeyword = True
  TokenCanBeConsumed StatementStart _ = False
  TokenCanBeConsumed SelectKeyword FromKeyword = True
  TokenCanBeConsumed SelectKeyword _ = False

-- type instance TokenCanBeConsumed '[ SelectKeyword ] FromKeyword = True
-- type instance TokenCanBeConsumed '[ SelectKeyword ] _ = False

----------------------------------------------------------------------------------------------------------------------------
-- class Context currentType previouslyVisitedTypes =
--   (->) Context (DeriveNewCurrentType previouslyVisitedTypes currentType) (AccumulateVisitedTypes previouslyVisitedTypes)
-- --  Context :: a -> Context (AccumulateVisitedTypes previouslyVisitedTypes a)


----------------------------------------------------------------------------------------------------------------------------
literal :: String -> Token

literal name = Literal name

----------------------------------------------------------------------------------------------------------------------------
select :: Token

select = SelectKeyword

----------------------------------------------------------------------------------------------------------------------------
where' :: Token

where' = WhereKeyword

----------------------------------------------------------------------------------------------------------------------------
from :: Token

from = FromKeyword


data TokenConsumptionAccumulator = forall lastConsumedToken . TokenConsumptionAccumulator [ Token ] (Proxy (lastConsumedToken :: Token))

----------------------------------------------------------------------------------------------------------------------------
class TokenConsumer consumer where
  consumeSqlTokens' :: TokenConsumptionAccumulator -> consumer

instance TokenConsumer TokenConsumptionAccumulator where
  consumeSqlTokens' (TokenConsumptionAccumulator tokens lastConsumedTokenProxy) = TokenConsumptionAccumulator (reverse tokens) lastConsumedTokenProxy

instance (TokenConsumer consumer) => TokenConsumer (Token -> consumer) where
  consumeSqlTokens' (TokenConsumptionAccumulator tokens lastConsumedTokenProxy) =
    \token -> case token of
      Literal string ->
        consumeSqlTokens' (TokenConsumptionAccumulator (token : tokens) (Proxy :: Proxy 'Literal))
      SelectKeyword ->
        consumeSqlTokens' (TokenConsumptionAccumulator (token : tokens) (Proxy :: Proxy 'SelectKeyword))


consumeSqlToken :: (TokenConsumer consumer) => consumer

consumeSqlToken = consumeSqlTokens' []



----------------------------------------------------------------------------------------------------------------------------
validQuery :: [ Token ]

validQuery = consumeSqlToken select

----------------------------------------------------------------------------------------------------------------------------
inValidQuery :: [ Token ]

inValidQuery = consumeSqlToken where'


--query = consumeSqlToken select (literal "c") from (literal "t") where'

----------------------------------------------------------------------------------------------------------------------------
-- optional "all"
--query = select all (literal "c") from (literal "t") where ("t" = "z")
