module Horn.FieldSet(
    FieldSet
) where

import              Horn.Field
import              Horn.DbType

--import              Record
--import              Record.Lens

--import              Data.Text
--import              Data.Vector
--import qualified    Data.Map.Strict as DMS
--
--import qualified    Hasql.Connection as HC
--import qualified    Hasql.Query as HQ
--import qualified    Hasql.Session as HS
--import qualified    Hasql.Encoders as HE
--import qualified    Hasql.Decoders as HD


data FieldSet = FieldSet {
    fields      :: Data.Set Field
}


joinFieldSet :: FieldSet -> FieldSet -> FieldSet



--
--
---- mathematically "pure", description,
---- ??should be suitable for storing calculated columns?? - probably should be promoted to anonymous Entities instead
---- to keep this class more "math-low-level"
--class (Monad self => FieldSet self) where
--
--    getFields :: some tree-based implementation for fields algebra
--
--    addFieldSet :: self -> FieldSet -> self
--
--    leftJoinFieldSet :: self -> FieldSet -> self
--
--    crossFieldSet :: self -> FieldSet -> self
--
--    unionFieldSet :: self -> FieldSet -> MayBe Entity
--
--    restrictFieldSetWhiteList :: self -> WhiteList -> self
--
--    restrictFieldSetBlackList :: self -> BlackList -> self
--
