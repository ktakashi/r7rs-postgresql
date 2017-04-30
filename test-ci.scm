(import (scheme base)
	(scheme write)
	(postgresql))

(cond-expand
 ((library (srfi 64))
  (import (srfi 64)))
 ((library (chibi test))
  ;; test-equal in (chibi test) is not SRFI-64 compatible
  ;; so wrap it
  (import (except (chibi test) test-equal))
  (begin
    (define-syntax test-equal
      (syntax-rules ()
	((_ name expect expr)
	 (test name expect expr))
	((_ expect expr)
	 (test-equal 'expr expect expr))))))
 ((library (gauche test))
  ;; gauche.test uses define-macro for test* so
  ;; it breaks hygine and we must need to import test
  ;; procedure as well...
  ;; I think it's a bug but this is how it is then it is...
  (cond-expand
   (gauche-0.9.4
    (import (rename (gauche test)
		    (test-start test-begin)
		    (test-section test-group))))
   (else
    (import (rename (only (gauche test) test-start test-section test* test-end)
		    (test-start test-begin)
		    (test-section test-group)))))
  (begin
    (define-syntax test-equal
      (syntax-rules ()
	((_ name expect expr)
	 (test* name expect expr))
	((_ expect expr)
	 (test-equal 'expr expect expr))))
    (define-syntax test-assert
      (syntax-rules ()
	((_ name expr)
	 (test* name #t expr))
	((_ expect expr)
	 (test-assert 'expr expr))))))
 (else
  (import (scheme write))
  (begin
    (define (test-begin . o) #f)

    (define (test-end . o) #f)

    (define-syntax test
      (syntax-rules ()
	((test expected expr)
	 (let ((res expr))
	   (cond
	    ((not (equal? expr expected))
	     (display "FAIL: ")
	     (write 'expr)
	     (display ": expected ")
	     (write expected)
	     (display " but got ")
	     (write res)
	     (newline))))))))))

(test-begin "R7RS PostgreSQL")
(define (print . args) (for-each display args) (newline))

;; user: postgres
;; pass: postgres
(define conn (make-postgresql-connection 
	      "localhost" "5432" #f "postgres" ""))

(test-assert "open connection" (postgresql-open-connection! conn))

(test-assert "try secure" (postgresql-secure-connection! conn))
(test-assert "login" (postgresql-login! conn))

;; may not be there yet (causes an error if there isn't)
(guard (e (else #t)) (postgresql-execute-sql! conn "drop table test"))
(test-assert "create tables"
	     (postgresql-execute-sql! conn
	       "create table test (id integer, name varchar(50))"))
(postgresql-execute-sql! conn "commit")
(test-assert "terminate" (postgresql-terminate! conn))

(test-assert (postgresql-open-connection! conn))
(test-assert "try secure" (postgresql-secure-connection! conn))
(test-assert (postgresql-login! conn))

(let ((r (postgresql-execute-sql! conn "select * from test")))
  (test-equal '#("id" "name")
	      (vector-map (lambda (v) (vector-ref v 0))
			  (postgresql-query-descriptions r)))
  (test-assert (not (postgresql-fetch-query! r))))

(postgresql-execute-sql! conn 
  "insert into test (id, name) values (1, 'name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (2, 'test name')")
(postgresql-execute-sql! conn 
  "insert into test (id, name) values (-1, 'test name2')")
(postgresql-execute-sql! conn  "commit")

(define (test-insert value)
  (let ((p (postgresql-prepared-statement 
	    conn "insert into test (id, name) values ($1, $2)")))
    (test-assert (postgresql-prepared-statement? p))
    (test-equal "insert into test (id, name) values ($1, $2)"
		(postgresql-prepared-statement-sql p))
    (test-assert (postgresql-bind-parameters! p 3 value))
    (test-assert 1 (postgresql-execute! p))
    (test-assert (postgresql-close-prepared-statement! p))))

(test-insert "name")
(test-insert '())

(let ((r (postgresql-execute-sql! conn "select * from test")))
  (test-equal '#(1 "name") (postgresql-fetch-query! r))
  (test-equal '#(2 "test name") (postgresql-fetch-query! r))
  (test-equal '#(-1 "test name2") (postgresql-fetch-query! r))
  (test-equal '#(3 "name") (postgresql-fetch-query! r))
  (test-equal '#(3 ()) (postgresql-fetch-query! r))
  (test-assert (not (postgresql-fetch-query! r))))

;; delete
(test-equal 5 (postgresql-execute-sql! conn "delete from test"))

;; max column test
(test-assert 
 (let ((p (postgresql-prepared-statement 
	   conn "insert into test (id, name) values ($1, $2)")))
   (let loop ((i 0))
     (unless (= i 100)
       (postgresql-bind-parameters! p i "name")
       (postgresql-execute! p)
       (loop (+ i 1))))
   (postgresql-close-prepared-statement! p)))
(postgresql-execute-sql! conn "commit")

(let ((p (postgresql-prepared-statement 
	  conn "select * from test where name = $1")))
  (postgresql-bind-parameters! p "name")
  (let ((q (postgresql-execute! p)))
    ;; skip first 50
    (do ((i 0 (+ i 1)))
	((= i 50))
      (postgresql-fetch-query! q))
    ;; 51
    (test-equal '#(50 "name") (postgresql-fetch-query! q))
    ;; skip next 48
    (do ((i 0 (+ i 1)))
	((= i 48))
      (postgresql-fetch-query! q))
    (test-equal "99" '#(99 "name") (postgresql-fetch-query! q))
    (test-assert (not (postgresql-fetch-query! q))))
  (postgresql-close-prepared-statement! p))

(let ((q (postgresql-execute-sql! conn "select * from test")))
  (do ((i 0 (+ i 1)))
      ((= i 60))
    (postgresql-fetch-query! q))
  (test-equal "60" '#(60 "name") (postgresql-fetch-query! q)))

(postgresql-execute-sql! conn "drop table test")
(postgresql-execute-sql! conn "commit")

(guard (e (else (test-assert (error-object? e))))
  (postgresql-execute-sql! conn "drop table test"))

;; issue #2 re-creation of prepared statement
;; ? is not a valid placeholder in PostgreSQL
(guard (e (else (test-assert (error-object? e))))
  (let ((ps (postgresql-prepared-statement 
             conn "select * from foo where a = ?")))
    (postgresql-close-prepared-statement! ps)
    (test-assert "? is not a valid syntax" #f)))
;; this hanged
(guard (e (else (test-assert (error-object? e))))
  (let ((ps (postgresql-prepared-statement 
             conn "select * from foo where a = ?")))
    (postgresql-close-prepared-statement! ps)
    (test-assert "Shouldn't be here" #f)))

;; terminate and close connection
(postgresql-terminate! conn)

(test-end)
