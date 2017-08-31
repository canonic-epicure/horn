module Horn.Entity(
    Entity
) where


---------
-- behavior of any relational entity
---------
class RelationalEntity a where
    select :: 
    project ::
    join :: 
    union ::
    intersect ::
    except ::



---------
-- some additional behavior of relational table
---------
class TableEntity a where
    select :: 
    project ::
    join :: 
    union ::
    intersect ::
    except ::
