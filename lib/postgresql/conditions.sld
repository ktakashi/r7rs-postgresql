;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/conditions.sld - Conditions
;;;  
;;;   Copyright (c) 2014-2017  Takashi Kato  <ktakashi@ymail.com>
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

(define-library (postgresql conditions)
  (export raise-postgresql-error postgresql-error?
	  postgresql-error-severity
	  postgresql-error-code
	  postgresql-error-schema
	  postgresql-error-table
	  postgresql-error-column
	  postgresql-error-data-type
	  postgresql-error-constraint)
  (cond-expand
   ((library (rnrs))
    (import (rnrs))
    (begin
      (define-condition-type &postgresql-error &error
	make-postgresql-error postgresql-error?
	(severity   postgresql-error-severity)
	(code       postgresql-error-code)
	(schema     postgresql-error-schema)
	(table      postgresql-error-table)
	(column     postgresql-error-column)
	(data-type  postgresql-error-data-type)
	(constraint postgresql-error-constraint))
      (define (raise-postgresql-error fields)
	(define (getf n)
	  (cond ((assv n fields) => cdr) (else #f)))
	(define (msg fields)
	  (define (get n)
	    (cond ((assv n fields) => cdr) (else "")))
	  ;; TODO should we also show source line or so?
	  (string-append (get #\S)
			 " [" (get #\C) "] "
			 (get #\M)
			 " at '" (get #\R) "' "
			 (get #\F) ":" (get #\L)))
	(raise (condition (make-message-condition (msg fields))
			  (make-postgresql-error (getf #\S)
						 (getf #\C)
						 (getf #\s)
						 (getf #\t)
						 (getf #\c)
						 (getf #\d)
						 (getf #\n)))))))
   (else
    (import (scheme base))
    (begin
      (define (postgresql-error? o) (error-object? o))
      (define-syntax define-error-accessors
	(syntax-rules ()
	  ((_) (begin))
	  ((_ (name defname) rest ...)
	   (begin
	     (define (defname e)
	       (define (parse irr)
		 (cond ((and irr (assq 'name irr)) => cdr)
		       (else #f)))
	       (and (postgresql-error? e)
		    (parse (error-object-irritants e))))
	     (define-error-accessors rest ...)))))
      (define-error-accessors
	(severity   postgresql-error-severity)
	(code       postgresql-error-code)
	(schema     postgresql-error-schema)
	(table      postgresql-error-table)
	(column     postgresql-error-column)
	(data-type  postgresql-error-data-type)
	(constraint postgresql-error-constraint))
      (define (raise-postgresql-error fields)
	(define (getf n)
	  (cond ((assv n fields) => cdr) (else #f)))
	(define (msg fields)
	  (define (get n)
	    (cond ((assv n fields) => cdr) (else "")))
	  ;; TODO should we also show source line or so?
	  (string-append (get #\S)
			 " [" (get #\C) "] "
			 (get #\M)
			 " at '" (get #\R) "' "
			 (get #\F) ":" (get #\L)))
	(error (msg fields)
	       `(severity   . ,(getf #\S))
	       `(code       . ,(getf #\C))
	       `(schema     . ,(getf #\s))
	       `(table      . ,(getf #\t))
	       `(column     . ,(getf #\c))
	       `(data-type  . ,(getf #\d))
	       `(constraint . ,(getf #\n))))
      ))))
