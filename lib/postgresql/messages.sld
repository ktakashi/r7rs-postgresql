;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/messages.sld - PostgreSQL protocol messages
;;;  
;;;   Copyright (c) 2014-2015  Takashi Kato  <ktakashi@ymail.com>
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

(define-library (postgresql messages)
  (export postgresql-send-startup-message
	  postgresql-send-password-message
	  postgresql-send-terminate-message
	  postgresql-send-sync-message
	  postgresql-send-flush-message
	  postgresql-send-query-message
	  postgresql-send-parse-message
	  postgresql-send-bind-message
	  postgresql-send-describe-message
	  postgresql-send-execute-message
	  postgresql-send-close-message
	  postgresql-send-copy-data-message
	  postgresql-send-copy-fail-message
	  postgresql-send-copy-done-message
	  postgresql-read-response)
  (import (scheme base) 
	  (scheme write)
	  (postgresql misc socket)
	  (postgresql misc bytevectors)
	  (postgresql misc io))
  (cond-expand
   ((library (srfi 19))
    (import (srfi 19)))
   (else 
    (begin 
      (define (time? o) #f)
      (define (time-utc->date t) t)
      (define (date? o) #f)
      (define (date->string d f) d)
      (define (date-nanosecond d) 0)
      (define (date-zone-offset d) 0))))
  (begin
    (define (send-s32 out v) (write-u32-be v out))
    (define (send-s16 out v) (write-u16-be v out))
    (define (send-string out s)
      (write-bytevector (string->utf8 s) out)
      ;; null terminate
      (write-u8 0 out))
    (define (send-bytes out bv)
      (write-bytevector bv out))

    ;; internal
    ;; - OUT must be an binary output port (socket-output-port)
    ;; - PARAMS must be alist. ((string . string) ...)
    (define (postgresql-send-startup-message out params)
      ;; calculate length
      ;; TODO check user is there...
      (define (calculate-length params)
	(let loop ((params params) (len 0) (r '()))
	  (if (null? params)
	      (values (+ len 9) (reverse r)) ;; length of param + int32*2 + term
	      (let ((name (string->utf8 (caar params)))
		    (value (string->utf8 (cdar params))))
		(loop (cdr params)
		      ;; null terminate ...
		      (+ len 
			 (bytevector-length name) 1
			 (bytevector-length value) 1)
		      (cons (cons name value) r))))))
      (let-values (((len params) (calculate-length params)))
	(send-s32 out len)
	(send-s32 out 196608) ;; magic number for protocol version 3
	(let loop ((params params))
	  (unless (null? params)
	    (send-bytes out (caar params))
	    (write-u8 0 out)
	    (send-bytes out (cdar params))
	    (write-u8 0 out)
	    (loop (cdr params))))
	(write-u8 0 out)
	(flush-output-port out)))

    ;; FIXME the same thing in apis.sld...
    (define (read-null-terminated-string params i)
      (let ((out (open-output-string)))
	(let loop ((i i))
	  (let ((b (bytevector-u8-ref params i)))
	    (if (zero? b)
		(values (+ i 1) (get-output-string out))
		(begin
		  (write-char (integer->char b) out)
		  (loop (+ i 1))))))))
    (define (parse-message-fields message)
      (let loop ((offset 0) (r '()))
	(let ((code (integer->char (bytevector-u8-ref message offset))))
	  (if (char=? code #\null)
	      r
	      (let-values (((offset value) 
			    (read-null-terminated-string message (+ offset 1))))
		(loop offset (cons (cons code value) r)))))))

    ;; it's not specified but all messages except startup
    ;; and ssl request start message type (byte1) and length (int32)
    (define (postgresql-read-response in)
      (let* ((ch (integer->char (read-u8 in)))
	     (size (bytevector->integer (read-bytevector 4 in)))
	     (payload (read-bytevector (- size 4) in)))
	(if (char=? ch #\E)
	    (let ((fields (parse-message-fields payload)))
	      (define (msg fields)
		(define (get n)
		  (cond ((assv n fields) => cdr) (else "")))
		;; TODO should we also show source line or so?
		(string-append (get #\S) " [" (get #\C) "] " (get #\M)
			       " at '" (get #\R) "' "(get #\F) ":" (get #\L)))
	      (error (msg fields)))
	    (values ch payload))))

    (define (postgresql-send-password-message out password)
      (let ((bv (if (string? password) (string->utf8 password) password)))
	(write-u8 (char->integer #\p) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1))
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))

    (define (postgresql-send-terminate-message out)
      (write-u8 (char->integer #\X) out)
      (send-s32 out 4)
      (flush-output-port out))

    (define (postgresql-send-sync-message out)
      (write-u8 (char->integer #\S) out)
      (send-s32 out 4)
      (flush-output-port out))

    (define (postgresql-send-flush-message out)
      (write-u8 (char->integer #\H) out)
      (send-s32 out 4)
      (flush-output-port out))

    (define (postgresql-send-query-message out sql)
      (let ((bv (string->utf8 sql)))
	(write-u8 (char->integer #\Q) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1))
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))

    (define (postgresql-send-describe-message out name type)
      (let ((bv (string->utf8 name)))
	(write-u8 (char->integer #\D) out)
	(send-s32 out (+ 4 1 (bytevector-length bv) 1))
	(write-u8 (char->integer type) out)
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))

    ;; prepared statement
    ;; TYPES must be a list. (type ...)
    ;; For now we don't suppor this so must always be '()
    (define (postgresql-send-parse-message out name sql types)
      (let ((sqlb (string->utf8 sql))
	    (nameb (if name (string->utf8 name) #u8(0)))
	    (typelen (length types)))
	(write-u8 (char->integer #\P) out)
	(send-s32 out (+ 4
			 (bytevector-length nameb) 1
			 (bytevector-length sqlb) 1
			 2 (* typelen 4)))
	(send-bytes out nameb)
	(write-u8 0 out)
	(send-bytes out sqlb)
	(write-u8 0 out)
	(send-s16 out typelen)
	(for-each (lambda (type) (send-s32 out type)) types)
	(flush-output-port out))
      )

    (define (postgresql-send-bind-message out portal prepared params formats)
      (define (->timestamp v)
	(let ((off (date-zone-offset v)))
	  (string-append
	   (date->string v "~Y-~m-~d ~H:~M:~S.")
	   ;; nano->milli
	   (number->string (remainder (date-nanosecond v) 1000000))
	   (if (negative? off)
	       ""
	       "+")
	   (number->string (/ off 3600)))))
      (define (->bytevector v) 
	(cond ((string? v) (string->utf8 v))
	      ((number? v) (string->utf8 (number->string v)))
	      ((bytevector? v) v)
	      ((date? v)   (string->utf8 (->timestamp v)))
	      ((time? v)   (->bytevector (time-utc->date v)))
	      (else 
	       (error "postgresql-send-bind-message: unsupported type" v))))
      ;; i think these must be one or the other but
      ;; document doesn't say it and we don't use portal.
      ;; so for now no check.
      (let ((portal (if portal (string->utf8 portal) #u8()))
	    (prepared (if prepared (string->utf8 prepared) #u8()))
	    (param-len (length params))
	    (formats-len 0 #;(length formats))
	    (params (map ->bytevector params)))
	(write-u8 (char->integer #\B) out)
	;; we use text ...
	(send-s32 out (+ 4 
			 (bytevector-length portal) 1
			 (bytevector-length prepared) 1
			 2 ;; parameter format codes
			 (* 2 param-len)
			 2 ;; parameter counts
			 (let loop ((r 0) (params params))
			   (if (null? params)
			       r
			       (loop (+ 4 (bytevector-length (car params)) r)
				     (cdr params))))
			 2 ;; result column format
			 ))
	(send-bytes out portal)
	(write-u8 0 out)
	(send-bytes out prepared)
	(write-u8 0 out)
	(send-s16 out param-len) ;; all text for now
	(let loop ((i 0))
	  (unless (= i param-len)
	    (send-s16 out 0)
	    (loop (+ i 1))))
	(send-s16 out param-len)
	(for-each (lambda (param)
		    (send-s32 out (bytevector-length param))
		    (send-bytes out param)
		    ;;(write-u8 0 out)
		    )
		  params)
	(send-s16 out formats-len)
	(flush-output-port out)))
	
    (define (postgresql-send-execute-message out name maxnum)
      (let ((bv (string->utf8 name)))
	(write-u8 (char->integer #\E) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1 4))
	(send-bytes out bv)
	(write-u8 0 out)
	(send-s32 out maxnum)
	(flush-output-port out)))

    (define (postgresql-send-close-message out type name)
      (let ((bv (string->utf8 name)))
	(write-u8 (char->integer #\C) out)
	(send-s32 out (+ 4 1 (bytevector-length bv) 1))
	(write-u8 (char->integer type) out)
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))

    ;; copy
    (define (postgresql-send-copy-fail-message out msg)
      (let ((bv (string->utf8 msg)))
	(write-u8 (char->integer #\f) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1))
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))

    (define (postgresql-send-copy-done-message out)
      (write-u8 (char->integer #\c) out)
      (send-s32 out 4)
      (flush-output-port out))

    (define (postgresql-send-copy-data-message out data)
      (write-u8 (char->integer #\d) out)
      (send-s32 out (+ 4 (bytevector-length data)))
      (send-bytes out data)
      (flush-output-port out))

    )

  )
