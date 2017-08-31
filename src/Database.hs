module GreenTara.Database(
) where

import GHC.TypeLits

import Data.Text

import GreenTara.Language




-- class SqlType a where
--     -- | Create a literal of this type.
--     mkLit :: a -> Lit a
  
--     -- | Default value when using 'def' at this type.
--     defaultValue :: Lit a
  
-- instance SqlType PostgresInt where
--     mkLit = LInt

--     defaultValue = LInt 0

-- type PostgresInt = Int

-- data Nullability = Nullable | NotNullable


-- data Database = Database {
-- }

-- data Schema = Schema {
-- }

-- data DbColumn (name :: Symbol) (nullability :: Nullability) sqlType 

-- data Table = Table {
--     column1     :: DbColumn "name" Nullable PostgresInt
-- }

-- data PrimaryKey