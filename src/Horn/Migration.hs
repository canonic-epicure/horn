module Horn.Migration(
    Migration
) where

{-| Migration plan

    Migration creates a new Entity (new table in DB?)

        migrate :: OldEntity -> Migration -> NewEntity

    all queries are adjusted to union all results from all tables
    in the background, tables are merged
-}