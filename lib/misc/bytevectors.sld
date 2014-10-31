;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; misc/bytevectors.sld - bytevector utilities
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

(define-library (misc bytevectors)
  (cond-expand
   (sagittarius
    (import (rnrs) (sagittarius))
    (begin
      (define (bytevector-u16-ref-le bv index)
	  (bytevector-u16-ref bv index (endianness little)))
      (define (bytevector-u32-ref-be bv index)
	(bytevector-u32-ref bv index (endianness big)))
      ))
   ((library (chibi bytevector))
    (import (chibi bytevector)))
   ((library (srfi 60))
    (import (scheme base) (srfi 60))
    (define (bytevector-u16-ref-le str i)
      (+ (bytevector-u8-ref str i)
	 (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 8)))
    (define (bytevector-u32-ref-be str i)
      (+ (arithmetic-shift (bytevector-u8-ref str i) 24)
	 (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 16)
	 (arithmetic-shift (bytevector-u8-ref str (+ i 2)) 8)
	 (bytevector-u8-ref str (+ i 3))))
    (define (bytevector->integer bv)
      (let ((len (bytevector-length bv)))
	(let lp ((i 0) (n 0))
	  (if (>= i len)
	      n
	      (lp (+ i 1)
		  (+ (arithmetic-shift n 8)
		     (bytevector-u8-ref bv i)))))))))
  (export bytevector-u16-ref-le bytevector-u32-ref-be bytevector->integer))
