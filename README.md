Introducing Horn — type safe relational algebra Haskell EDSL, with effective SQL backend and schema import.
=====================================

I’m glad to announce about the initial release of Horn — relational algebra Haskell EDSL.

Horn has the following design choices:
- Be typesafe — should not be possible to write a query, referencing a non-existed column in the table for example.
- Provide more OOP-like look and feel to the business of SQL entities querying (minimum boilerplate for trivial operations).
- Allow to express arbitrary SQL query (up to vendor specifics)
- Support nifty SQL extensions like “INSERT RETURNING”
- Support streaming.


It assumes the authoritative source of the schema to be the currently one in the DB. It comes with schema import tool, which generates a Haskell source files with schema definition and user is advised to use external tool for managing and visualizing his/her DB schema.

Relation algebra intuition
--------------------------

Relation algebra is a math behind the SQL language. It can be thought as a regular arithmetic operations, though the targets of the operations are not the usual numbers, but different objects. 
Those objects are then roughly like a row in the RDBMS table,