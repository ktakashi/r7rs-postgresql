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
	(write-u8 0 out)))
    
    ;; it's not specified but all messages except startup
    ;; and ssl request start message type (byte1) and length (int32)
    (define (postgresql-read-response in)
      (let* ((ch (integer->char (read-u8 in)))
	     (size (bytevector->integer (read-bytevector 4 in)))
	     (payload (read-bytevector (- size 4) in)))
	(if (char=? ch #\E)
	    (let ((code (read-u8 in)))
	      (if (zero? code) ;; terminate just raise an error
		  (error "read-response: unknown error")
		  (let ((code-char (string (integer->char code))))
		    (error (string-append "read-response: " code-char)
			   (utf8->string payload 1)))))
	    (values ch payload))))

    (define (postgresql-send-password-message out password)
      (let ((bv (if (string? password) (string->utf8 password) password)))
	(write-u8 (char->integer #\p) out)
	(send-s32 out (+ 4 (bytevector-length bv) 1))
	(send-bytes out bv)
	(write-u8 0 out)))
    )

  )
