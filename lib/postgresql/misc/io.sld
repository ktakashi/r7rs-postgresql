;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/misc/io.sld - I/O utilities
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

(define-library (postgresql misc io)
  (import (scheme base) (scheme case-lambda))
  (cond-expand
   (sagittarius
    (import (binary io) (only (rnrs) endianness))
    (begin
      (define write-u16-be
	(case-lambda 
	 ((value) (write-u16-be value (current-output-port)))
	 ((value out) (put-u16 out value (endianness big)))))
      (define write-u32-be
	(case-lambda 
	 ((value) (write-u32-be value (current-output-port)))
	 ((value out) (put-u32 out value (endianness big)))))))
   (else
    (cond-expand
     ;; well chibi only has srfi 33...
     ((library (srfi 60))
      (import (srfi 60)))
     ((library (srfi 33))
      (import (srfi 33))))
    (begin
      ;; it's all big endian
      (define (write-nbytes out value n)
	(let loop ((r '()) (v value) (i 0))
	  (if (= i n)
	      (for-each (lambda (b) (write-u8 b out)) r)
	      (let ((b (bitwise-and v #xFF)))
		(loop (cons b r) (arithmetic-shift v -8) (+ i 1))))))
      (define write-u16-be
	(case-lambda
	 ((value) (write-u16-be value (current-output-port)))
	 ((value out) (write-nbytes out value 2))))
      (define write-u32-be
	(case-lambda
	 ((value) (write-u32-be value (current-output-port)))
	 ((value out) (write-nbytes out value 4)))))))
  (export write-u16-be write-u32-be))
