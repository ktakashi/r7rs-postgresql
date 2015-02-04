PostgreSQL binding for R7RS Scheme
==================================

This is a PostgreSQL socket frontend interface library written in pure
R7RS Scheme.

NOTE: it's still working state.


Supported implementations
-------------------------

- Sagittarius 0.5.9 or later
- Gauche 0.9.4
- Chibi Scheme 0.7

This library should work in R7RS implementations which support
the following SRFIs:

- SRFI-60/SRFI-33 or R6RS library `(rnrs)`
- SRFI-106
- SRFI-19 (Optional)


High level APIs
---------------

### Library

- `(postgresql)` The library provides high level APIs to communicate
  with PostgreSQL.


### Procedures

- `(postgresql-connection? obj)`

  Returns `#t` if _obj_ is an PostgreSQL connection object.


-  `(make-postgresql-connection host port database username password)`

   `database` can be `#f`.

   All arguments must be a string except `database`. Creates a
   PostgreSQL connection. At this moment, the connection to the server
   is *not* established.


- `(postgresql-open-connection! conn)`

  Establishes a connection with specified _conn_ object.


- `(postgresql-login! conn)`

  Logging in to the PostgreSQL server.


- `(postgresql-terminate! conn)`

  Terminates the session and disconnects from the server.


- `(postgresql-prepared-statement? obj)`

  Return `#t` if _obj_ is a PostgreSQL prepared statement.


- `(postgresql-prepared-statement conn sql)`

  Creates a prepared statement object.


- `(postgresql-close-prepared-statement! prepared-statement)`

  Closes the prepared statement _prepared-statement_.


- `(postgresql-bind-parameters! prepared-statement . params)`

  Binds parameter _params_ to given _prepared-statement_.


- `(postgresql-execute! prepared-statement)`

  Executes the given _prepared-statement_ and returns either
  PostgreSQL query object for SELECT statement or affected row count.

  To retrieve the result, use the `postgresql-fetch-query!` procedure.


- `(postgresql-query? obj)`

  Returns `#t` if _obj_ is a PostgreSQL query object.


- `(postgresql-execute-sql! conn sql)`

  Executes the given _sql_ statement. If _sql_ is a select statement
  then the value returned is a PostgreSQL query object. Otherwise
  `#t`.  This procedure retrieves all results in one go if _sql_ is a
  SELECT statement. So it may cause memory explosion if the result set
  is too big.


- `(postgresql-fetch-query! query)`

  Fetch a row as a vector. If no more data are available, then returns
  `#f`.


- `(postgresql-start-transaction! conn mode)`

  Issue `START TRANSACTION` statement to start a transaction.  _mode_
  specifies how the transation should be.

  The argument _mode_ must be either a PostgreSQL transaction mode
  object or `#f`.


- `(postgresql-transaction-mode alist)`

  Creates a PostgreSQL transaction mode object. The _alist_ specifies
  how the transaction mode is created. It may have the following
  symbols as its key.

  - `isolation-level`
  - `access-mode`
  - `deferrable`

  Each key must have one of the followings:

  For `isolation-level`:

  - Variable: `postgresql-isolation-level-serializable`
  - Variable: `postgresql-isolation-level-repeatable-read`
  - Variable: `postgresql-isolation-level-read-committed`
  - Variable: `postgresql-isolation-level-read-uncommitted`

  For `access-mode`:

  - Variable: `postgresql-access-mode-read-write`
  - Variable: `postgresql-access-mode-read-only`

  For `deferrable`:

  - Variable: `postgresql-deferrable-on`
  - Variable: `postgresql-deferrable-off`


- `(postgresql-commit! conn)`

  Issue `COMMIT` statement.


- `(postgresql-rollback! conn)`

  Issue `ROLLBACK` statement.

### Parameters
- `*postgresql-maximum-results*`

  Configuration parameter for how many result it should fetch. Default
  value is 50.


- `*postgresql-copy-data-handler*`

  Handler of COPY to stdout command. The value must be a procedure and
  takes 2 arguments, data type and data. The data type should be one
  of the the following symbols:

  - header
  - data
  - complete

  When the data type is `header` then the given data is a list of data
  information. It contains 3 elements, the format of overall COPY
  command, 0 is textual, 1 is binary.

  When the data type is `data` then the given data is a bytevector
  whose content is the result of COPY command.

  When the data type is `complete` then the given data is `#f`. This
  indicates the COPY command is done.


- `*postgresql-write-data-handler*`

 Handler of COPY from stdin command. The value must be a procedure and
 takes 2 arguments, data type and data. The data type could be one of
 the following symbols;

 - header
 - data
 - complete

  When the data type is `header` then the given data is a list of data
  information. It contains 3 elements, the format of overall COPY
  command, 0 is textual, 1 is binary.

  When the data type is `data` then the given data is a `#f`. When
  there is no more data to send, then the handler must return `#f`
  otherwise it would go into inifinite loop.

  When the data type is `complete` then the given data is `#t`. This indicates
  the COPY command is done.


  These handlers are currently a thin wrapper of the COPY
  command. Using them, users need to know about how the data is
  sent. For more detail, please refer the PostgreSQL manual.


Low level APIs
--------------

TBD


Data conversion
---------------

Data conversion is done automatically by high level APIs. The following table
describes how it's done.

| PostgreSQL type |     Scheme type      |
|:--------------- | --------------------:|
|   Integers      |   Number             |
|   Float         |   Inexact number     |
|   Characters    |   String             |
|   Date          |   SRFI-19 date       |
|   Time          |   SRFI-19 date       |
|   Timestamp     |   SRFI-19 time       |
|   UUID          |   String             |

_Note_: If the implementation doesn't support SRFI-19, the scheme type
will be string.


TODO
----

- Data conversion is not done properly
  - need some document but couldn't find it...
- ~~Prepared statement~~
- ~~Maybe buffering~~
  - ~~currently it can't execute second query unless it fetches all records.~~
