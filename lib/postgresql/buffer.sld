
;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/buffer.sld - PostgreSQL message buffer
;;;  
;;;   Copyright (c) 2014-2016  Takashi Kato  <ktakashi@ymail.com>
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

;; explicit buffering layer.
;; This library provides buffering of socket port.
;; (NB: implmented atop custom port would be cleaner if we could use.)
(define-library (postgresql buffer)
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
	  postgresql-read-response


	  make-postgresql-out-buffer
	  )
  (import (scheme base)
	  (scheme write)
	  (prefix (postgresql messages) m:))
  (begin

    (define-record-type postgresql-out-buffer
      (%make-postgresql-out-buffer sock-out buffer)
      postgresql-out-buffer?
      (sock-out %bo)
      ;; R7RS doesn't have any manner to truncate the output-bytevector
      ;; so not sure if this actually improves the performance.
      ;; (Needs to be measured)
      (buffer %bb %bb!))
    (define (make-postgresql-out-buffer sock-out)
      (%make-postgresql-out-buffer sock-out (open-output-bytevector)))

    (define-syntax define-forward
      (syntax-rules ()
	((_ name real) (define-forward name real (buffer)))
	((_ name real (buffer args ...))
	 (define (name buffer args ...)
	   (real (%bo buffer) args ...)))))
    (define-syntax define-buffering
      (syntax-rules ()
	((_ name real) (define-buffering name real (buffer)))
	((_ name real (buffer args ...))
	 (define (name buffer args ...)
	   (real (%bb buffer) args ...)))))
	
    (define-forward postgresql-send-startup-message
      m:postgresql-send-startup-message
      (out messages))
    (define-forward postgresql-send-password-message
      m:postgresql-send-password-message
      (out password))
    (define-forward postgresql-send-terminate-message
      m:postgresql-send-terminate-message)
    (define-forward postgresql-send-sync-message
      m:postgresql-send-sync-message)
    
    (define (postgresql-send-flush-message out)
      (write-bytevector (get-output-bytevector (%bb out)) (%bo out))
      (close-output-port (%bb out))
      (%bb! out (open-output-bytevector))
      (m:postgresql-send-flush-message (%bo out)))
    
    (define-buffering postgresql-send-query-message 
      m:postgresql-send-query-message
      (out sql))
    (define-buffering postgresql-send-parse-message
      m:postgresql-send-parse-message
      (out name sql types))
    (define-buffering postgresql-send-bind-message
      m:postgresql-send-bind-message
      (out portal prepared params formats))
    (define-buffering postgresql-send-describe-message
      m:postgresql-send-describe-message
      (out name type))
    (define-buffering postgresql-send-execute-message
      m:postgresql-send-execute-message
      (out name maxnum))
    (define-buffering postgresql-send-close-message
      m:postgresql-send-close-message
      (out type name))
    (define-buffering postgresql-send-copy-data-message
      m:postgresql-send-copy-data-message
      (out date))
    (define-buffering postgresql-send-copy-fail-message
      m:postgresql-send-copy-fail-message
      (out msg))
    (define-buffering postgresql-send-copy-done-message
      m:postgresql-send-copy-done-message)
    
    (define postgresql-read-response m:postgresql-read-response)
    )
  )
