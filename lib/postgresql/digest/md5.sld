;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/digest/md5.sld - PostgreSQL MD5 hash
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

;; TODO should we return string???
(define-library (postgresql digest md5)
  (import (scheme base))
  (cond-expand
   (sagittarius
    (import (math) (postgresql misc bytevectors) (rnrs))
    (begin
      (define (md5 src)
	(let ((bv (cond ((string? src) (string->utf8 src))
			((bytevector? src) src)
			(else
			 (error "md5: must be string or bytevector" src)))))
	  
	  (bytevector->hex-string (hash MD5 bv))))))
   (else
    (cond-expand
     ((library (rnrs))
      (import (rename (only (rnrs)
			    bitwise-and bitwise-ior bitwise-xor
			    bitwise-arithmetic-shift)
		      (bitwise-arithmetic-shift arithmetic-shift))))
     ((library (srfi 60))
      (import (srfi 60)))
     ((library (srfi 33))
      (import (srfi 33)))
     (else (begin (error '(digest md5) "bitwise library is required"))))
    (import (postgresql misc bytevectors))
    (include "md5.scm")
    #;
    (begin
      (define (%md5 src)
	(let ((s (md5 src)))
	  (hex-string->bytevector s))))))
  (export md5 #;(rename %md5 md5)
	  ))
