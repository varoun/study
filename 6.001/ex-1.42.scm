;;; Exercise 1.42 - function composition

(define (compose f g)
  (lambda (x) (f (g x))))

#|
> (define (square n) (* n n))
; no values returned
> (define (inc n) (+ n 1))
; no values returned
>
> ((compose square inc) 6)
49
> 
|#
