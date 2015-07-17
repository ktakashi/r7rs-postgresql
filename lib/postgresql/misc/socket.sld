;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/misc/socket.sld - socket utilities
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

(define-library (postgresql misc socket)
  (cond-expand
   ((library (srfi 106))
    (import (srfi 106)))
   (chibi
    ;; only support what we need.
    (import (scheme base) (chibi net) (scheme cxr) (chibi filesystem))
    (begin
      ;; well...
      (define socket? list?)
      (define (make-client-socket host port . opt)
	(let ((r (open-net-io host port)))
	  (unless r (error "make-client-socket: failed to create a socket"))
	  r))
      (define (socket-input-port sock) (cadr sock))
      (define (socket-output-port sock) (caddr sock))
      (define (socket-close sock) (close-file-descriptor (car sock)))
      ;; do nothing for now
      (define (socket-shutdown sock how) #t)
      )))
  (cond-expand
   (gauche (import (gauche base))
	   (begin (define socket? (with-module srfi-106 socket?))))
   (else))
  (export socket? make-client-socket socket-input-port socket-output-port
	  socket-close socket-shutdown))
