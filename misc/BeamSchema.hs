{-# LANGUAGE StandaloneDeriving, GADTs, ScopedTypeVariables,
  FlexibleContexts, FlexibleInstances, DeriveGeneric,
  TypeSynonymInstances #-}
module Schema (db, migration) where
import Database.Beam
import Control.Applicative
import Data.Void (type Void)
import Database.Beam.Migrate
       (createTable, field, int, maybeType, notNull, runMigrationSilenced,
        unCheckDatabase, type CheckedDatabaseSettings, type Migration,
        type Sql92SaneDdlCommandSyntax)

data SomeTableT f = SomeTable{_someTableName ::
                              Columnar f (Maybe Void),
                              _someTableId :: Columnar f Int}
                  deriving Generic

instance Beamable SomeTableT

type SomeTable = SomeTableT Identity

deriving instance Show SomeTable

deriving instance Eq SomeTable

instance Table SomeTableT where
        data PrimaryKey SomeTableT f = SomeTableKey (Columnar f Int)
                                     deriving Generic
        primaryKey = SomeTableKey <$> _someTableId

type SomeTableKey = PrimaryKey SomeTableT Identity

instance Beamable (PrimaryKey SomeTableT)

deriving instance Eq SomeTableKey

deriving instance Show SomeTableKey

data Table2T f = Table2{_table2Id :: Columnar f Int,
                        _table2FkColumn :: Columnar f (Maybe Int)}
               deriving Generic

instance Beamable Table2T

type Table2 = Table2T Identity

deriving instance Show Table2

deriving instance Eq Table2

instance Table Table2T where
        data PrimaryKey Table2T f = Table2Key (Columnar f Int)
                                  deriving Generic
        primaryKey = Table2Key <$> _table2Id

type Table2Key = PrimaryKey Table2T Identity

instance Beamable (PrimaryKey Table2T)

deriving instance Eq Table2Key

deriving instance Show Table2Key

data Db entity = Db{_someTable :: entity (TableEntity SomeTableT),
                    _table2 :: entity (TableEntity Table2T)}
               deriving Generic

migration ::
            Sql92SaneDdlCommandSyntax syntax =>
            Migration syntax (CheckedDatabaseSettings be Db)
migration
  = do someTable <- createTable "someTable"
                      (SomeTable
                         (field "name"
                            (maybeType
                               (error
                                  "Unknown type: PG type PgDataTypeDescrOid (Oid 25) Nothing")))
                         (field "id" int notNull))
       table2 <- createTable "table2"
                   (Table2 (field "id" int notNull)
                      (field "fkColumn" (maybeType int)))
       pure Db{_someTable = someTable, _table2 = table2}

instance Database Db

db ::
   forall be syntax hdl m .
     (MonadBeam syntax be hdl m, Sql92SaneDdlCommandSyntax syntax) =>
     DatabaseSettings be Db
db = unCheckDatabase (runMigrationSilenced (migration @syntax))
