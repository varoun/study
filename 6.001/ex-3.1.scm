;;;; Exercise 3.1 -- make-accumulator

(define (make-accumulator initval)
  (lambda (inc) 
    (set! initval (+ initval inc))
    initval))
#|
> (define A (make-accumulator 5))
; no values returned
> (A 10)
15
> (A 10)
25
> A
#{Procedure 8588 (unnamed in make-accumulator)}
> 
|#
