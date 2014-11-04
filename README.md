PostgreSQL binding for R7RS Scheme
==================================

This is a PostgreSQL socket frontend interface library written in pure
R7RS Scheme.

NOTE: it's still working state.


Supporting implementations
--------------------------

- Sagittarius 0.5.9 or later
- Gauche 0.9.4
- Chibi Scheme 0.7

This library should be portable for R7RS if the implementation supports 
the following SRFIs;

- SRFI-60/SRFI-33 or R6RS library `(rnrs)`
- SRFI-106
- SRFI-19 (Optional)


High level APIs
---------------

Library: `(postgresql)`

The library provides high level APIs to communicate PostgreSQL.


Procedure: `(postgresql-connection? obj)`

Returns `#t` if _obj_ is an PostgreSQL connection object.


Procedure: `(make-postgresql-connection host port database username password)`

`database` can be `#f`.

All arguments must be a string except `database`. Creates a PostgreSQL
connection. At this moment, the connection to the server is *not* established.


Procedure: `(postgresql-open-connection! conn)`

Establishes a connection with specified _conn_ object.


Procedure: `(postgresql-login! conn)`

Logging in to the PostgreSQL server.


Procedure: `(postgresql-terminate! conn)`

Terminates the session and disconnects connection.


Procedure: `(postgresql-prepared-statement? obj)`

Return `#t` if _obj_ is a PostgreSQL prepared statement.


Procedure: `(postgresql-prepared-statement conn sql)`

Creates a prepared statement object.


Procedure: `(postgresql-close-prepared-statement! prepared-statement)`

Closes prepared statement.


Procedure: `(postgresql-bind-parameters! prepared-statement . params)`

Binds parameter _params_ to given _prepared-statement_.


Procedure: `(postgresql-execute! prepared-statement)`

Executes the given _prepared-statement_ and returns either PostgreSQL
query object for SELECT statement or affected row count.

To retrieve the result, use `postgresql-fetch-query!` procedure.


Procedure: `(postgresql-query? obj)`

Returns `#t` if _obj_ is a PostgreSQL query object.


Procedure: `(postgresql-execute-sql! conn sql)`

Executes the given _sql_. If the _sql_ is a select statement then
the returning value is a PostgreSQL query object. Otherwise `#t`.
This procedure retrieves all result in one go if the _sql_ is a SELECE
statement. So it may cause memory explosion if the result set is
too big.


Procedure: `(postgresql-fetch-query! query)`

Fetch a row as a vector. If no more data are available, then returns `#f`.


Procedure: `(postgresql-start-transaction! conn mode)`

Issue `BEGIN` statement to start transaction. _mode_ specifies how the
transation should be.

NOTE: _mode_ is not implemented correctly yet.


Procedure: `(postgresql-commit! conn)`

Issue `COMMIT` statement.


Procedure: `(postgresql-rollback! conn)`

Issue `ROLLBACK` statement.


Parameter: `*postgresql-maximum-results*`

Configureation parameter for how many result it should fetch. Default
value is 50.


Parameter: `*postgresql-copy-data-handler*`

Handler of COPY to stdout command. The value must be a procedure and
takes 2 arguments, data type and data. The data type could be the
following symbols;

- header
- data
- complete

When the data type is `header` then the given data is a list of data
information. It contains 3 elements, the format of overall COPY command,
0 is textual, 1 is binary.

When the data type is `data` then the given data is a bytevector whose
content is the result of COPY command.

When the data type is `complete` then the given data is `#f`. This indicates
the COPY command is done.


Low level APIs
--------------

TBD


Data conversion
---------------

Data conversion is done automatically by high level APIs. Following table
describes how it's done.

| PostgreSQL type |     Scheme type    |
|:--------------- | ------------------:|
|   Integers      |   Number           |
|   Float         |   Inexact number   |
|   Characters    |   String           |
|   Date          |   SRFI-19 date[^*] |
|   Time          |   SRFI-19 date[^*] |
|   Timestamp     |   SRFI-19 time[^*] |
|   UUID          |   String           |

[^*]: If the implementation supports SRFI-19, otherwise string.


TODO
----

- Data conversion is not done properly
  - need some document but couldn't find it...
- ~~Prepared statement~~
- ~~Maybe buffering~~
  - ~~currently it can't execute second query unless it fetches all records.~~
