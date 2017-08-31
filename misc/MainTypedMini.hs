{-# OPTIONS_GHC -fprint-explicit-kinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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


data MyType = Var1 | Var2

data Some

value :: MyType

value = Var1



type' :: Proxy (a :: MyType)

type' = Proxy :: Proxy Var1


listOfValues :: [ MyType ]

listOfValues = [ Var1, Var2 ]


data SomeTypeValue = forall (a :: MyType) . SomeTypeValue (Proxy a)


listOfTypes :: [ SomeTypeValue ]

listOfTypes = [ SomeTypeValue (Proxy :: Proxy 'Var1), SomeTypeValue (Proxy :: Proxy 'Var2), SomeTypeValue (Proxy :: Proxy Some) ]


-----------------------------------------------------------------------------
main :: IO ()
main = print "main"





-- --import Data.Proxy

-- import MainTypedMini

-- main :: IO ()
-- main = print "yo"


-- data Token =
--   Literal String
--   | SelectKeyword
--   | FromKeyword
--   | WhereKeyword

--   deriving Show


-- -- ----------------------------------------------------------------------------------------------------------------------------
-- -- type family AccumulateVisitedTypes previosulyVisitedTypes newType

-- -- type instance AccumulateVisitedTypes [] a =
-- --   [ a ]

-- -- type instance AccumulateVisitedTypes previous a =
-- --   a : previous

-- -- ----------------------------------------------------------------------------------------------------------------------------
-- -- type family DeriveNewCurrentType previosulyVisitedTypes newType

-- -- type instance DeriveNewCurrentType a = ()


-- ----------------------------------------------------------------------------------------------------------------------------
-- -- class Context currentType previouslyVisitedTypes =
-- --   (->) Context (DeriveNewCurrentType previouslyVisitedTypes currentType) (AccumulateVisitedTypes previouslyVisitedTypes)
-- -- --  Context :: a -> Context (AccumulateVisitedTypes previouslyVisitedTypes a)


-- ----------------------------------------------------------------------------------------------------------------------------
-- literal :: String -> Token

-- literal name = Literal name

-- ----------------------------------------------------------------------------------------------------------------------------
-- select :: Token

-- select = SelectKeyword

-- ----------------------------------------------------------------------------------------------------------------------------
-- where' :: Token

-- where' = WhereKeyword

-- ----------------------------------------------------------------------------------------------------------------------------
-- from :: Token

-- from = FromKeyword

-- ----------------------------------------------------------------------------------------------------------------------------
-- class TokenConsumer consumer where
--   consumeSqlTokens' :: [ Token ] -> consumer

-- instance TokenConsumer [ Token ] where
--   consumeSqlTokens' accumulator = reverse accumulator

-- instance (TokenConsumer consumer) => TokenConsumer (Token -> consumer) where
--   consumeSqlTokens' accumulator = \token -> consumeSqlTokens' (token : accumulator)


-- consumeSqlToken :: (TokenConsumer consumer) => consumer

-- consumeSqlToken = consumeSqlTokens' []


-- ----------------------------------------------------------------------------------------------------------------------------
-- validQuery :: [ Token ]

-- validQuery = consumeSqlToken select from

-- ----------------------------------------------------------------------------------------------------------------------------
-- inValidQuery :: [ Token ]

-- inValidQuery = consumeSqlToken select where'


-- --query = consumeSqlToken select (literal "c") from (literal "t") where'

-- ----------------------------------------------------------------------------------------------------------------------------
-- -- optional "all"
-- --query = select all (literal "c") from (literal "t") where ("t" = "z")
--                -- 
