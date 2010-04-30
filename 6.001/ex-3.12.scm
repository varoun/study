;;;; Exercise 3.12 - append!

;; The last-pair procedure - 
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

#|
> (last-pair '(a b c d))
'(d)
> 
|#

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

#|

> (define x (list 'a 'b))
; no values returned
> (define y (list 'c 'd))
; no values returned
> (define z (append x y))
; no values returned
> x
'(a b)
> y
'(c d)
> (define w (append! x y))
; no values returned
> w
'(a b c d)
> x
'(a b c d)
> y
'(c d)
> 
|#
