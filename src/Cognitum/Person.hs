module Cognitum.Person(
    
) where

-- collection of person-related methods
import Cognitum.Schema.Person



instance TableInterface Person where
    all :: Stream Person

    restrict :: Condition

