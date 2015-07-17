(import (scheme base)
	(postgresql digest md5)
	(postgresql misc bytevectors)
	(postgresql misc socket)
	(postgresql messages)
	(postgresql))

(cond-expand
 ((library (srfi 64))
  (import (srfi 64)))
 ((library (chibi test))
  ;; test-equal in (chibi test) contains a bug
  ;; (the macro can't be expanded properly)
  ;; so make thin wrapper
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

(test-begin "Misc")
;; for Gauche
(test-assert "socket?" (not (not socket?)))
(test-end)


(test-begin "PostgreSQL")

(test-begin "Digest MD5")

(test-equal "d41d8cd98f00b204e9800998ecf8427e"
	    (md5 ""))
(test-equal "900150983cd24fb0d6963f7d28e17f72"
	    (md5 "abc"))
(test-equal "9e107d9d372bb6826bd81d3542a419d6"
	    (md5 "The quick brown fox jumps over the lazy dog"))

(test-end)

;; TODO more tests
(test-begin "Protocol messages")

(let ((out (open-output-bytevector)))
  (postgresql-send-startup-message out '(("user" . "postgres")))
  (test-equal #u8(0 0 0 23
		  0 3 0 0 
		  117 115 101 114 0 
		  112 111 115 116 103 114 101 115 0
		  0)
	      (get-output-bytevector out)))

(let ((out (open-output-bytevector)))
  (postgresql-send-password-message out "password")
  (test-equal #u8(112 ;; #\p
		  0 0 0 13
		  112 97 115 115 119 111 114 100 0)
	      (get-output-bytevector out)))

(let ((out (open-output-bytevector)))
  (postgresql-send-terminate-message out)
  (test-equal #u8(88 ;; #\X
		  0 0 0 4)
	      (get-output-bytevector out)))

(test-end)

;; TODO more tests
(test-begin "APIs")

(test-assert "connection?" 
	     (postgresql-connection? 
	      (make-postgresql-connection "localhost" "5432" 
					  #f "postgres" "postgres")))


(test-end)

(test-end)
