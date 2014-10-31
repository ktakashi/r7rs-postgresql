(import (scheme base)
	(postgresql))

;; user: postgres
;; pass: postgres
(define conn (make-postgresql-connection 
	      "localhost" "5432" #f "postgres" "postgres"))

;; open the connection
(postgresql-open-connection! conn)

;; login
(postgresql-login conn)
