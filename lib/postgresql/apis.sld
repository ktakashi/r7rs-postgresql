;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/apis.sld - PostgreSQL API
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(define-library (postgresql apis)
  (export make-postgresql-connection
	  postgresql-connection?
	  postgresql-open-connection!
	  postgresql-login!
	  postgresql-terminate!

	  ;; simple query
	  postgresql-query?
	  postgresql-query-descriptions
	  postgresql-execute-sql!
	  postgresql-fetch-query!
	  )
  (import (scheme base)
	  (scheme write)
	  (scheme char)
	  (postgresql messages)
	  (digest md5)
	  (misc socket)
	  (misc bytevectors))
  (begin
    (define-record-type postgresql-connection 
      (make-postgresql-connection host port database username password)
      postgresql-connection?
      (host     postgresql-connection-host)
      (port     postgresql-connection-port)
      (database postgresql-connection-database)
      (username postgresql-connection-username)
      (password postgresql-connection-password)
      ;; after it's opened
      (socket   postgresql-connection-socket postgresql-connection-socket-set!)
      ;; 
      (sock-in  postgresql-connection-sock-in
		postgresql-connection-sock-in-set!)
      (sock-out postgresql-connection-sock-out 
		postgresql-connection-sock-out-set!)
      (params   postgresql-connection-params postgresql-connection-params-set!)
      (id       postgresql-connection-id postgresql-connection-id-set!)
      (key      postgresql-connection-key postgresql-connection-key-set!))

    (define (postgresql-open-connection! conn)
      (let ((s (make-client-socket (postgresql-connection-host conn)
				   (postgresql-connection-port conn))))
	(postgresql-connection-socket-set! conn s)
	(postgresql-connection-sock-in-set! conn (socket-input-port s))
	(postgresql-connection-sock-out-set! conn (socket-output-port s))
	conn))

    (define (close-conn conn)
      (socket-close (postgresql-connection-socket conn)))

    ;; can be used anywhere
    (define (read-null-terminated-string params i)
      (let ((out (open-output-string)))
	(let loop ((i i))
	  (let ((b (bytevector-u8-ref params i)))
	    (if (zero? b)
		(values (+ i 1) (get-output-string out))
		(begin
		  (write-char (integer->char b) out)
		  (loop (+ i 1))))))))

    (define (postgresql-login! conn)
      (define read-string read-null-terminated-string)
      (define (store-params params)
	(define len (bytevector-length params))
	;; convert it to alist ((name . value)) 
	;; name is symbol, value is string
	(let loop ((i 0) (r '()))
	  (let*-values (((next name) (read-string params i))
			((next value) (read-string params next)))
	    (if (= len next)
		(postgresql-connection-params-set! conn r)
		(loop next (cons (cons (string->symbol name) value) r))))))
      (define (next in)
	(let-values (((code payload) (postgresql-read-response in)))
	  (case code
	    ((#\K)
	     (let ((id (bytevector-u32-ref-be payload 0))
		   (key (bytevector-u32-ref-be payload 4)))
	       (postgresql-connection-id-set! conn id)
	       (postgresql-connection-key-set! conn key)
	       (next in)))
	    ((#\S) 
	     (store-params payload)
	     (next in))
	    ((#\N)
	     (let ((code (bytevector-u8-ref payload 0)))
	       ;; TODO how should we treat this?
	       (unless (zero? code)
		 (write-string (utf8->string payload 1) (current-error-port))
		 (newline (current-error-port)))
	       (next in)))
	    ((#\Z) #t))))

      (let ((in   (postgresql-connection-sock-in conn))
	    (out  (postgresql-connection-sock-out conn))
	    (user (postgresql-connection-username conn))
	    (pass (postgresql-connection-password conn))
	    (database (let ((d (postgresql-connection-database conn)))
			(if d (list (cons "database" d)) '()))))
	(postgresql-send-startup-message out (cons (cons "user" user) database))
	;; authenticate
	(let loop ((first #t))
	  ;; concat('md5', md5(concat(md5(concat(password, username)),
	  ;;                          random-salt)))
	  ;; PostgreSQL md5 function returns hex string in small letters,
	  ;; so we need to do some trick here.
	  ;; it horribly sucks but it's inevitable...
	  (define (construct payload)
	    (let* ((pu5 (md5 (string-append pass user)))
		   (pus5 (md5 (bytevector-append (string->utf8 pu5) 
						 (bytevector-copy payload 4)))))
	      (string-append "md5" pus5)))
	  (define (send-password conn payload)
	    (unless first
	      (error "postgresql-login!: failed to login"))
	    (if (= (bytevector-length payload) 4)
		(postgresql-send-password-message out pass)
		(postgresql-send-password-message out (construct payload))))
	  (let-values (((code payload) (postgresql-read-response in)))
	    (unless (char=? code #\R)
	      (close-conn conn)
	      (error "postgresql-login!: server respond unexpected message"
		     code))
	    ;; get content
	    (case (bytevector-u32-ref-be payload 0)
	      ((0) (next in)) ;; ok
	      ((3) (send-password conn payload) (loop #f))
	      ((5) (send-password conn payload) (loop #f))
	      (else 
	       (close-conn conn)
	       (error "postgresql-login!: unsupported login method")))))))

    (define (postgresql-terminate! conn)
      (let ((out (postgresql-connection-sock-out conn)))
	(postgresql-send-terminate-message out)
	(close-conn conn)))

    (define-record-type postgresql-query
      (make-postgresql-query connection eoq)
      postgresql-query?
      (connection postgresql-query-connection)
      (descriptions postgresql-query-descriptions 
		    postgresql-query-descriptions-set!)
      ;; end of query
      (eoq          postgresql-query-eoq 
		    postgresql-query-eoq-set!))

    ;; parse description to a vector
    ;; a description:
    ;;  #(name table-id column-num type-id type-size type-modifier format-code)
    (define (parse-row-description query payload)
      (define read-string read-null-terminated-string)
      (let* ((n (bytevector-u16-ref-be payload 0))
	     (vec (make-vector n #f)))
	(let loop ((offset 2) (i 0))
	  (if (= i n)
	      (begin (postgresql-query-descriptions-set! query vec) query)
	      (let-values (((next name) (read-string payload offset)))
		(let ((table-id   (bytevector-u32-ref-be payload next))
		      (column-num (bytevector-u16-ref-be payload (+ next 4)))
		      (type-id    (bytevector-u32-ref-be payload (+ next 6)))
		      (type-size  (bytevector-u16-ref-be payload (+ next 10)))
		      (type-mod   (bytevector-u32-ref-be payload (+ next 12)))
		      (fmt-code   (bytevector-u16-ref-be payload (+ next 16))))
		  (vector-set! vec i (vector name table-id column-num type-id
					     type-size type-mod fmt-code))
		  (loop (+ next 18) (+ i 1))))))))

    (define (postgresql-execute-sql! conn sql)
      (let ((out (postgresql-connection-sock-out conn))
	    (in  (postgresql-connection-sock-in conn)))
	(postgresql-send-query-message out sql)
	;; get 
	(let loop ((r #t))
	  (let-values (((code payload) (postgresql-read-response in)))
	    (case code
	      ((#\C)           ;; Close
	       (when (postgresql-query? r)
		 (postgresql-query-eoq-set! r #t))
	       (loop r))
	      ((#\Z) r)        ;; ReadyForQuery
	      ((#\T)	       ;; RowDescription
	       ;; TODO should we store records?
	       (let ((query (make-postgresql-query conn #f)))
		 (parse-row-description query payload)))
	      (else (loop r)))))))

    (define (parse-record query payload)
      (define (read-fix payload offset size)
	(let ((end (+ offset size)))
	  (values end (bytevector-copy payload offset end))))

      (define (convert value type)
	;; i need something...
	(case type
	  ;; integer
	  ((23) (string->number (utf8->string value)))
	  ;; varchar
	  ((1043) (utf8->string value))
	  ;; else (just return for now)
	  (else value)))

      (let* ((n (bytevector-u16-ref-be payload 0))
	     (vec (make-vector n #f))
	     (desc (postgresql-query-descriptions query)))
	(let loop ((offset 2) (i 0))
	  (if (= i n)
	      vec
	      (let ((size (bytevector-u32-ref-be payload offset))
		    (type (vector-ref (vector-ref desc i) 3))
		    (offset (+ offset 4)))
		;; I hope this is the only negative number
		;; or should we check if the most significat bit is set?
		(let-values (((offset value) (read-fix payload offset size)))
		  (vector-set! vec i (convert value type))
		  (loop offset (+ i 1))))))))

    (define (postgresql-fetch-query! query)
      (if (postgresql-query-eoq query)
	  #f
	  (let loop ((in (postgresql-connection-sock-in 
			  (postgresql-query-connection query))))
	    (let-values (((code payload) (postgresql-read-response in)))
	      (case code
		((#\C) (postgresql-query-eoq-set! query #t) (loop in))
		((#\Z) #f)
		((#\D) (parse-record query payload))
		(else 
		 (error "postgresql-fetch-query!: unexpected code" code)))))))

    )
)
