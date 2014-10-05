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

(define-syntax define-chunk-vector
  (ir-macro-transformer
   (lambda (exp inj cmp)
     (apply
      (lambda (_ <prefix> <type-string> <make-vector> <vector-type>)

	(define %chunk-size
	  `(foreign-lambda unsigned-integer "dv_vector_chunk_size" chunk-vector))

	`(begin

	   ;; (make-<type>chunk-vector size [size-hint])
	   ;; Create a new chunk vector with a chunk-size of /size/.
	   (define (,(symbol-append 'make- (inj <prefix>) 'chunk-vector)
		    size #!optional (size-hint 64))
	     (set-finalizer!
	      ((foreign-lambda chunk-vector "dv_vector_new"
			       unsigned-integer unsigned-integer)
	       (* size (foreign-type-size ,(inj <type-string>)))
	       size-hint)
	      (foreign-lambda void "dv_vector_free" chunk-vector)))
	   

	   ;; (<type>vector-remove! vector index)
	   ;; Removes a chunk from the vector using its /index/.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-remove!)
	     (foreign-lambda void "dv_vector_remove" chunk-vector unsigned-integer))

	   ;; (<type>vector-set! vectror index value)
	   ;; Changed the value of a chunk using its /index/.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-set!)
	     (foreign-lambda void "dv_vector_change" chunk-vector 
			     unsigned-integer ,(inj <vector-type>)))

	   ;; (<type>vector-push! vector value)
	   ;; Pushes a new chunk to the vector.
	   (define (,(symbol-append (inj <prefix>) 'chunk-vector-push!)
		    chunk-vector data)
	     (let ((grown (make-u32vector 1)))
	       (values
		((foreign-lambda unsigned-integer "dv_vector_push" 
				 chunk-vector ,(inj <vector-type>) u32vector)
		 chunk-vector data grown)
		(u32vector-ref grown 0))))

	   ;; (<type>vector-ref vector index)
	   ;; Returns the data at /index/.
	   (define (,(symbol-append (inj <prefix>) 'chunk-vector-ref)
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
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector-length)
	     (foreign-lambda unsigned-integer "dv_vector_size" chunk-vector))

	   ;; <type>vector->pointer
	   ;; Returns a pointer to the dense foreign array where the data
	   ;; is stored.
	   (define ,(symbol-append (inj <prefix>) 'chunk-vector->pointer)
	     (foreign-lambda c-pointer "dv_vector_data" chunk-vector))

	   )) exp))))

(define-chunk-vector f32 "float"    make-f32vector f32vector)
(define-chunk-vector f64 "double"   make-f64vector f64vector)
(define-chunk-vector s8  "int8_t"   make-s8vector  s8vector)
(define-chunk-vector s16 "int16_t"  make-s16vector s16vector)
(define-chunk-vector s32 "int32_t"  make-s32vector s32vector)
(define-chunk-vector u8  "uint8_t"  make-u8vector  u8vector)
(define-chunk-vector u16 "uint16_t" make-u16vector u16vector)
(define-chunk-vector u32 "uint32_t" make-u32vector u32vector)

)
