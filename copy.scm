;; example how to do copy data.

(import (scheme base)
	(scheme write)
	(postgresql))

(define conn (make-postgresql-connection 
	      "localhost" "5432" #f "postgres" "postgres"))

(define (print . args) (for-each display args) (newline))

(postgresql-open-connection! conn)
(postgresql-login! conn)

(guard (e (else #t)) (postgresql-execute-sql! conn "drop table test"))
(guard (e (else (print (error-object-message e))))
  (postgresql-execute-sql! conn
    "create table test (id integer not null primary key, name varchar(50))"))

(define (copy-handler type payload)
  (case type
    ((header) (write payload) (newline))
    ((data)   (display (utf8->string payload)))))

(define (write-handler n)
  (let ((count 0))
    (lambda (type data)
      (case type
	((data)
	 (set! count (+ count 1))
	 (if (not (= count n)) 
	     (string->utf8 (string-append (number->string count)
					  "\tdata\n"))
	     #f))))))

(*postgresql-copy-data-handler* copy-handler)
(*postgresql-write-data-handler* (write-handler 100))

(guard (e (else (print e)))
  (print (postgresql-execute-sql! conn  "copy test from stdin")))

(print (postgresql-execute-sql! conn  "copy test to stdout with delimiter ','"))

(postgresql-terminate! conn)
