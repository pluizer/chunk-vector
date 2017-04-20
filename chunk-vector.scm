#|
Copyright (c) 2014 Richard van Roy (pluizer)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#

;; A dyn-vector like library capable of storing other srfi-4 vectors inside a dense array.

(module chunk-vector
*
(import chicken scheme foreign)
(use srfi-4 lolevel)

#>
#include "cdynvector.h"
#include "stdint.h"
<#

(define-foreign-type chunk-vector (c-pointer "DV_Vector"))

(define-syntax %define-chunk-vector
  (ir-macro-transformer
   (lambda (exp inj cmp)
     (apply
      (lambda (_ <prefix> <type-string> <make-vector> <vector-type>)

	(define %chunk-size
	  `(foreign-lambda unsigned-integer "dv_vector_chunk_size" chunk-vector))

        (define (compose-name . args)
          (inj (apply symbol-append (map strip-syntax args))))

	`(begin

	   ;; (make-<type>chunk-vector chunk-size [size-hint])
	   ;; Create a new chunk vector with a chunk-size of /size/.
	   (define (,(compose-name 'make- <prefix> 'chunk-vector)
		    chunk-size #!optional (size-hint 64))
	     (set-finalizer!
	      ((foreign-lambda chunk-vector "dv_vector_new"
			       unsigned-integer unsigned-integer)
	       (* chunk-size (foreign-type-size ,(inj <type-string>)))
	       size-hint)
	      (foreign-lambda void "dv_vector_free" chunk-vector)))
	   
	   ;; (<type>vector-remove! vector index)
	   ;; Removes a chunk from the vector using its /index/.
	   (define ,(compose-name <prefix> 'chunk-vector-remove!)
	     (foreign-lambda void "dv_vector_remove" chunk-vector unsigned-integer))

	   ;; (<type>vector-set! vectror index value)
	   ;; Changed the value of a chunk using its /index/.
	   (define ,(compose-name <prefix> 'chunk-vector-set!)
	     (foreign-lambda void "dv_vector_change" chunk-vector 
			     unsigned-integer ,(inj <vector-type>)))

	   ;; (<type>vector-push! vector value)
	   ;; Pushes a new chunk to the vector.
	   (define (,(compose-name <prefix> 'chunk-vector-push!)
		    chunk-vector data)
	     (let ((grown (make-u32vector 1)))
	       ((foreign-lambda unsigned-integer "dv_vector_push" 
				chunk-vector ,(inj <vector-type>) u32vector)
		chunk-vector data grown)))

	   ;; (<type>vector-ref vector index)
	   ;; Returns the data at /index/.
	   (define (,(compose-name <prefix> 'chunk-vector-ref)
		    chunk-vector index)
	     (let* ((chunk-size (,%chunk-size chunk-vector))
		    (size (/ chunk-size (foreign-type-size ,(inj <type-string>))))
		    (r (,(inj <make-vector>) size)))
	       ((foreign-lambda* void ((,(inj <vector-type>) r)
				       (chunk-vector v)
				       (unsigned-integer i)
				       (unsigned-integer s))  "
	   		void* t = dv_vector_ref(v, i);
	   		memcpy(r, t, s);")  
		r chunk-vector index chunk-size) r))

	   ;; (<type>vector-length vector)
	   ;; Returns the number of chunks in the vector.
	   (define ,(compose-name <prefix> 'chunk-vector-length)
	     (foreign-lambda unsigned-integer "dv_vector_size" chunk-vector))

	   ;; <type>vector->pointer
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(compose-name <prefix> 'chunk-vector->pointer)
	     (foreign-lambda c-pointer "dv_vector_data" chunk-vector))

	   ;; <type>vector-chunk-size
	   ;; Returns the size of the chunk.
	   (define ,(compose-name <prefix> 'chunk-vector-chunk-size)
	     (foreign-lambda unsigned-integer "dv_vector_chunk_size" chunk-vector))

	   ;; <type>vector-clear!
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(compose-name <prefix> 'chunk-vector-clear!)
	     (foreign-lambda void "dv_vector_clear" chunk-vector))

	   )) exp))))

(%define-chunk-vector f32 "float"    make-f32vector f32vector)
(%define-chunk-vector f64 "double"   make-f64vector f64vector)
(%define-chunk-vector s8  "int8_t"   make-s8vector  s8vector)
(%define-chunk-vector s16 "int16_t"  make-s16vector s16vector)
(%define-chunk-vector s32 "int32_t"  make-s32vector s32vector)
(%define-chunk-vector u8  "uint8_t"  make-u8vector  u8vector)
(%define-chunk-vector u16 "uint16_t" make-u16vector u16vector)
(%define-chunk-vector u32 "uint32_t" make-u32vector u32vector)

;; General
(define (make-chunk-vector type chunk-length #!optional (size-hint 64))
  (let* ((funcs
	  (case type
	    ((char: int8: byte:) 
	     (list make-s8chunk-vector s8chunk-vector-push!
		   s8chunk-vector-remove! s8chunk-vector-set! 
		   s8chunk-vector-ref s8chunk-vector-length
		   s8chunk-vector->pointer s8chunk-vector-chunk-size
		   s8chunk-vector-clear!))
	    ((uchar: uint8: unsigned-byte:)
	     (list make-u8chunk-vector u8chunk-vector-push!
		   u8chunk-vector-remove! u8chunk-vector-set! 
		   u8chunk-vector-ref u8chunk-vector-length
		   u8chunk-vector->pointer u8chunk-vector-chunk-size
		   u8chunk-vector-clear!))
	    ((short: int16:)
	     (list make-s16chunk-vector s16chunk-vector-push!
		   s16chunk-vector-remove! s16chunk-vector-set! 
		   s16chunk-vector-ref s16chunk-vector-length
		   s16chunk-vector->pointer s16chunk-vector-chunk-size
		   s16chunk-vector-clear!))
	    ((ushort: uint16: unsigned-short:)
	     (list make-u16chunk-vector u16chunk-vector-push!
		   u16chunk-vector-remove! u16chunk-vector-set! 
		   u16chunk-vector-ref u16chunk-vector-length
		   u16chunk-vector->pointer u16chunk-vector-chunk-size
		   u16chunk-vector-clear!))
	    ((int: int32: integer: integer32:)
	     (list make-s32chunk-vector s32chunk-vector-push!
		   s32chunk-vector-remove! s32chunk-vector-set! 
		   s32chunk-vector-ref s32chunk-vector-length
		   s32chunk-vector->pointer s32chunk-vector-chunk-size
		   s32chunk-vector-clear!))
	    ((uint: uint32: unsigned-int: unsigned-int32:
		    unsigned-make-integer: unsigned
		    integer: unsigned-integer32:)
	     (list make-u32chunk-vector u32chunk-vector-push!
		   u32chunk-vector-remove! u32chunk-vector-set!
		   u32chunk-vector-ref u32chunk-vector-length
		   u32chunk-vector->pointer u32chunk-vector-chunk-size
		   u32chunk-vector-clear!))
	    ((float: float32:)
	     (list make-f32chunk-vector f32chunk-vector-push!
		   f32chunk-vector-remove! f32chunk-vector-set! 
		   f32chunk-vector-ref f32chunk-vector-length
		   f32chunk-vector->pointer f32chunk-vector-chunk-size
		   f32chunk-vector-clear!))
	    ((double: float64:)
	     (list make-f64chunk-vector f64chunk-vector-push!
		   f64chunk-vector-remove! f64chunk-vector-set! 
		   f64chunk-vector-ref f64chunk-vector-length
		   f64chunk-vector->pointer f64chunk-vector-chunk-size
		   f64chunk-vector-chunk-size
		   f64chunk-vector-clear!))))
	 (vector ((car funcs) chunk-length size-hint)))
    (apply (lambda (_ push! remove! set! ref length pointer chunk-size clear)
	     (lambda (com #!rest args)
	       (apply (case com
			((push!) push!)
			((remove!) remove!)
			((set!) set!)
			((ref) ref)
			((length) length)
			((pointer) pointer)
			((chunk-size) chunk-size)
			((type) (lambda (_) type))
			((clear!) clear)
			(else (assert #f)))
		      (cons vector args))))
	   funcs)))

(define (chunk-vector-remove! vector index)
  (vector 'remove! index))

(define (chunk-vector-push! vector value)
  (vector 'push! value))

(define (chunk-vector-set! vector index value)
  (vector 'set! index value))

(define (chunk-vector-ref vector index)
  (vector 'ref index))

(define (chunk-vector-length vector)
  (vector 'length))

(define (chunk-vector->pointer vector)
  (vector 'pointer))

(define (chunk-vector-chunk-size vector)
  (vector 'chunk-size))

(define (chunk-vector-type vector)
  (vector 'type))

(define (chunk-vector-clear! vector)
  (vector 'clear!))

)
