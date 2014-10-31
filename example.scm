(import (scheme base)
	(scheme write)
	(postgresql))

(define (print . args) (for-each display args) (newline))

;; user: postgres
;; pass: postgres
(define conn (make-postgresql-connection 
	      "localhost" "5432" #f "postgres" "postgres"))

(print "open connection")
;; open the connection
(postgresql-open-connection! conn)

;; login
(print "login")
(postgresql-login! conn)

(print "simple query")
(postgresql-execute-sql! conn "drop table test")
(postgresql-execute-sql! conn "create table test (id integer, name varchar(50))")

(let ((r (postgresql-execute-sql! conn "select * from test")))
  (print (postgresql-query-descriptions r))
  (print (postgresql-fetch-query! r)))

(postgresql-execute-sql! conn 
  "insert into test (id, name) values (1, 'name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (2, 'test name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (-1, 'test name2')")

(let ((r (postgresql-execute-sql! conn "select * from test")))
  (print (postgresql-query-descriptions r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r)))

;; terminate and close connection
(print "terminate")
(postgresql-terminate! conn)
