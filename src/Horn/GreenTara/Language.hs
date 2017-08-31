{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, GADTs #-}

module GreenTara.Language(
    SqlLiteral(..),
    SqlOperation(..),
    SqlLanguage
) where

import Data.Text

import Data.Comp.Multi
import Data.Comp.Multi.Show ()
import Data.Comp.Multi.Equality ()
import Data.Comp.Multi.Ordering ()
import Data.Comp.Multi.Derive

----------------------------------------------------------------------------------------------------------------------------
data TSqlKeyword

data SqlKeyword (ext :: * -> *) typeLabel where
    SELECT          :: SqlKeyword ext TSqlKeyword
    COLLATE         :: SqlKeyword ext TSqlKeyword
    KEYWORD         :: !Text -> SqlKeyword ext TSqlKeyword


data SqlLiteral (ext :: * -> *) typeLabel where
    SqlLiteralInt       :: !Int     -> SqlLiteral ext Int
    SqlLiteralText      :: !Text    -> SqlLiteral ext Text

data SqlOperation (ext :: * -> *) typeLabel where
    SqlOperationAdd, SqlOperationMult   :: ext typeLabel -> ext typeLabel -> SqlOperation ext typeLabel


data TSqlStatementSelect

data SqlStatementSelect (ext :: * -> *) typeLabel where
    SqlStatementSelectSimple    :: Int -> SqlStatementSelect ext TSqlStatementSelect
    SqlStatementSelectCombined  :: ext TSqlStatementSelect -> ext TSqlStatementSelect -> SqlStatementSelect ext TSqlStatementSelect


data TSqlStatement

data SqlStatement (ext :: * -> *) typeLabel where
    SqlStatementSelect          :: ext TSqlStatementSelect -> SqlStatement ext TSqlStatement
    SqlStatementOp              :: ext (SqlOperation ext Int) -> SqlStatement ext TSqlStatement

  
type SqlLanguage = SqlOperation :+: SqlLiteral :+: SqlStatementSelect :+: SqlStatement

$(derive 
    [makeHFunctor, makeHFoldable, makeHTraversable, makeShowHF, makeEqHF, makeOrdHF, smartConstructors, smartAConstructors] 
    [''SqlLiteral, ''SqlOperation, ''SqlStatementSelect, ''SqlStatement ]
    )

----------------------------------------------------------------------------------------------------------------------------
-- Term evaluation algebra
class EvalI f where
    evalAlgI :: Alg f I
  
$(derive [liftSum] [''EvalI])

-- Lift the evaluation algebra to a catamorphism
evalI :: (HFunctor f, EvalI f) => Term f i -> i
evalI = unI . cata evalAlgI

instance EvalI SqlLiteral where
    evalAlgI (SqlLiteralInt n) = I n
    evalAlgI (SqlLiteralText n) = I n

instance EvalI SqlOperation where
    evalAlgI (SqlOperationAdd (I x) (I y))  = I (x + y)
    evalAlgI (SqlOperationMult (I x) (I y)) = I (x * y)

instance EvalI SqlStatementSelect 
-- where
--     evalAlgI (SqlStatementSelect (SqlLiteralInt int))  = I int
--     evalAlgI (SqlOperationMult (I x) (I y)) = I (x * y)

-- Example: evalEx = 2
evalIEx :: Int
evalIEx = evalI (iSqlOperationAdd (iSqlLiteralInt 2) (iSqlLiteralInt 1) :: Term SqlLanguage Int)


-- data SqlExpression = SqlExpression [ SqlStatement ]

-- data SqlStatement = InsertStatement | SelectStatement {
--     sColumns        :: [ ColumnExpression ],
--     sSources        :: [ SourceExpression ],
--     sWhere          :: [ BoolExpression ]
-- }

-- -- Signature for values and operators
-- data Value a i where
--     Const ::        Int -> Value a Int
--     Pair  :: a i -> a j -> Value a (i,j)
--   data Op a i where
--     Add, Mult :: a Int -> a Int   -> Op a Int
--     Fst       ::          a (i,j) -> Op a i
--     Snd       ::          a (i,j) -> Op a j
  
--   -- Signature for the simple expression language
--   type Sig = Op :+: Value
  
--   -- Derive boilerplate code using Template Haskell (GHC 7 needed)
--   $(derive [makeHFunctor, makeHFoldable, makeHTraversable, makeShowHF, makeEqHF,
--             makeOrdHF, smartConstructors, smartAConstructors] 
--            [''Value, ''Op])


-- -- Term evaluation algebra
-- class EvalI f where
--     evalAlgI :: Alg f I
  
--   $(derive [liftSum] [''EvalI])
  
--   -- Lift the evaluation algebra to a catamorphism
--   evalI :: (HFunctor f, EvalI f) => Term f i -> i
--   evalI = unI . cata evalAlgI
  
--   instance EvalI Value where
--     evalAlgI (Const n) = I n
--     evalAlgI (Pair (I x) (I y)) = I (x,y)
  
--   instance EvalI Op where
--     evalAlgI (Add (I x) (I y))  = I (x + y)
--     evalAlgI (Mult (I x) (I y)) = I (x * y)
--     evalAlgI (Fst (I (x,_)))    = I x
--     evalAlgI (Snd (I (_,y)))    = I y
  
--   -- Example: evalEx = 2
--   evalIEx :: Int
--   evalIEx = evalI (iFst $ iPair (iConst 2) (iConst 1) :: Term Sig Int)
