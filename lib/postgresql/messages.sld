;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/messages.sld - PostgreSQL protocol messages
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

(define-library (postgresql messages)
  (export postgresql-send-startup-message
	  postgresql-send-password-message
	  postgresql-send-terminate-message
	  postgresql-send-query-message
	  postgresql-read-response)
  (import (scheme base) 
	  (scheme write)
	  (misc socket)
	  (misc bytevectors)
	  (misc io))
  (begin
    (define (send-s32 out v) (write-u32-be v out))
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
      (send-s32 out 4))

    (define (postgresql-send-query-message out sql)
      (let ((bv (string->utf8 sql)))
	(write-u8 (char->integer #\Q) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1))
	(send-bytes out bv)
	(write-u8 0 out)
	(flush-output-port out)))
    )

  )
