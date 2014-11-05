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

(print "create tables")
;; may not be there yet (causes an error if there isn't)
(guard (e (else #t)) (postgresql-execute-sql! conn "drop table test"))
(guard (e (else (print (error-object-message e))))
  (postgresql-execute-sql! conn
    "create table test (id integer, name varchar(50))"))
(postgresql-terminate! conn)

(postgresql-open-connection! conn)
(postgresql-login! conn)

(print "simple query")
(let ((r (postgresql-execute-sql! conn "select * from test")))
  (print (postgresql-query-descriptions r))
  (print (postgresql-fetch-query! r)))

(postgresql-execute-sql! conn 
  "insert into test (id, name) values (1, 'name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (2, 'test name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (-1, 'test name2')")
(postgresql-execute-sql! conn  "commit")

(print "insert with prepared statement")
(let ((p (postgresql-prepared-statement 
	  conn "insert into test (id, name) values ($1, $2)")))
  (print (postgresql-prepared-statement-sql p))
  (print (postgresql-bind-parameters! p 3 "name"))
  (let ((q (postgresql-execute! p)))
    (print q))
  (postgresql-close-prepared-statement! p))

(let ((r (postgresql-execute-sql! conn "select * from test")))
  (print (postgresql-query-descriptions r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r))
  (print (postgresql-fetch-query! r)))

(let ((p (postgresql-prepared-statement 
	  conn "select * from test where name = $1")))
  (print (postgresql-prepared-statement-sql p))
  (print (postgresql-bind-parameters! p "name"))
  (let ((q (postgresql-execute! p)))
    (print q)
    (print (postgresql-fetch-query! q))
    (print (postgresql-fetch-query! q)))
  (postgresql-close-prepared-statement! p))

(let ((p (postgresql-prepared-statement 
	  conn "select * from test where id = $1")))
  (print (postgresql-prepared-statement-sql p))
  (print (postgresql-bind-parameters! p 1))
  (let ((q (postgresql-execute! p)))
    (print q)
    (print (postgresql-fetch-query! q))
    (print (postgresql-fetch-query! q)))
  (postgresql-close-prepared-statement! p))

;; delete
(print "delete")
(print (postgresql-execute-sql! conn "delete from test"))

;; max column test
(let ((p (postgresql-prepared-statement 
	  conn "insert into test (id, name) values ($1, 'name')")))
  (let loop ((i 0))
    (unless (= i 100)
      (postgresql-bind-parameters! p i)
      (postgresql-execute! p)
    (loop (+ i 1))))
  (postgresql-close-prepared-statement! p))

(let ((p (postgresql-prepared-statement 
	  conn "select * from test where name = $1")))
  (print (postgresql-prepared-statement-sql p))
  (print (postgresql-bind-parameters! p "name"))
  (let ((q (postgresql-execute! p)))
    ;; skip first 50
    (print "skip 50")
    (do ((i 0 (+ i 1)))
	((= i 50))
      (postgresql-fetch-query! q))
    ;; 51
    (print "get 51st")
    (print (postgresql-fetch-query! q))
    ;; skip next 50
    (do ((i 0 (+ i 1)))
	((= i 50))
      (postgresql-fetch-query! q))
    (print (postgresql-fetch-query! q)))
  (postgresql-close-prepared-statement! p))

(let ((q (postgresql-execute-sql! conn "select * from test")))
  (do ((i 0 (+ i 1)))
      ((= i 60))
    (postgresql-fetch-query! q))
  (print (postgresql-fetch-query! q)))

(postgresql-execute-sql! conn "drop table test")

(print "droping non existing table")
(guard (e (else (print (error-object-message e))))
  (postgresql-execute-sql! conn "drop table test"))

;; terminate and close connection
(print "terminate")
(postgresql-terminate! conn)
