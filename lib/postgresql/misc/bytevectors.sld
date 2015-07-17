;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; postgresql/misc/bytevectors.sld - bytevector utilities
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

(define-library (postgresql misc bytevectors)
  (cond-expand
   (sagittarius
    (import (rnrs)
	    (sagittarius) 
	    (sagittarius control))
    (begin
      (define (bytevector-u16-ref-le bv index)
	  (bytevector-u16-ref bv index (endianness little)))
      (define (bytevector-u16-ref-be bv index)
	  (bytevector-u16-ref bv index (endianness big)))
      (define (bytevector-u32-ref-be bv index)
	(bytevector-u32-ref bv index (endianness big)))
      (define (bytevector->hex-string bv)
	(call-with-string-output-port
	 (lambda (out)
	   (dotimes (i (bytevector-length bv))
	     (format out "~2,'0x" (bytevector-u8-ref bv i))))))))
   ((library (chibi bytevector))
    (import (scheme base) (chibi bytevector)))
   (else
    (cond-expand
     ((library (srfi 60))
      (import (srfi 60)))
     ((library (srfi 33))
      (import (srfi 33))))
    (import (scheme base) (scheme char))
    (begin
      (define (bytevector-u16-ref-le str i)
	(+ (bytevector-u8-ref str i)
	   (arithmetic-shift (bytevector-u8-ref str (+ i 1)) 8)))
      (define (bytevector-u16-ref-be str i)
	(+ (arithmetic-shift (bytevector-u8-ref str i) 8)
	   (bytevector-u8-ref str (+ i 1))))
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
		       (bytevector-u8-ref bv i)))))))
      (define (hex-string->bytevector str)
	;; make the same as following
	;; (integer->bytevector (string->number str 16))
	;; so it needs to handle odd length string as well
	;; thus "123" would be #vu8(#x01 #x23)
	(define (safe-ref s i)
	  (if (< i 0) #\0 (string-ref s i)))
	(define (->hex c)
	  (if (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
			#\A #\B #\C #\D #\E #\F
			#\a #\b #\c #\d #\e #\f))
	      (or (digit-value c) ;; easy
		  (let ((c (char-upcase c)))
		    ;; must be A-F
		    (- (char->integer c) #x37)))
	      (error "hex-string->bytevector: non hex character" c str)))
	(let* ((len (string-length str))
	       (bv (make-bytevector (ceiling (/ len 2)))))
	  (let loop ((i (- (bytevector-length bv) 1)) (j (- len 1)))
	    (if (< i 0)
		bv
		(let ((h (->hex (safe-ref str (- j 1))))
		      (l (->hex (safe-ref str j))))
		  (bytevector-u8-set! bv i 
				      (bitwise-ior (arithmetic-shift h 4) l))
		  (loop (- i 1) (- j 2)))))))
      (define (integer->hex-string n)
	(let* ((res (number->string n 16))
	       (len (string-length res)))
	  (if (even? len)
	      res
	      (string-append "0" res))))
      (define (bytevector->hex-string bv)
	(let ((out (open-output-string))
	      (len (bytevector-length bv)))
	  (let lp ((i 0))
	    (cond
	     ((>= i len)
	      (get-output-string out))
	     (else
	      (write-string (integer->hex-string (bytevector-u8-ref bv i)) out)
	      (lp (+ i 1))))))))))
  (cond-expand
   (sagittarius
    (import (util bytevector)))
   (else
    (begin
      ;; we don't need much thing for this
      (define (bytevector-prefix-length bv1 bv2 start1 end1 start2 end2)
	(let* ((delta (min (- end1 start1) (- end2 start2)))
	       (end1 (+ start1 delta)))
	  (if (and (eq? bv1 bv2) (= start1 start2))	; EQ fast path
	      delta
	      (let lp ((i start1) (j start2))		; Regular path
		(if (or (>= i end1)
			(not (= (bytevector-u8-ref bv1 i)
				(bytevector-u8-ref bv2 j))))
		    (- i start1)
		    (lp (+ i 1) (+ j 1)))))))
      
      (define (bytevector-prefix? bv1 bv2)
	(let* ((end1 (bytevector-length bv1))
	       (end2 (bytevector-length bv2))
	       (len1 (- end1 0)))
	  (and (<= len1 (- end2 0))
	       (= (bytevector-prefix-length bv1 bv2 0 end1 0 end2) len1)))))))

  (export bytevector-u16-ref-le bytevector-u16-ref-be 
	  bytevector-u32-ref-be
	  bytevector->integer
	  bytevector-prefix?
	  hex-string->bytevector bytevector->hex-string))
