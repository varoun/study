;;;; Exercise 3.13 - Circular lists

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

#|
> (define z (make-cycle (list 'a 'b 'c 'd)))
; no values returned
> 
Asking scheme the val of "z" would make it print an infinite stream of
a b c d 's
> (length z) ;; goes into infinite cdring
  C-c C-c
Condition: (&interrupt keyboard)
1> 
|#

