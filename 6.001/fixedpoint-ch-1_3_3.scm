;;; Fixed point of a function - Section 1.3.3

;; the tolerance
(define tolerance 0.00001)

;; fixed point search
(define (fixed-point f initial-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try initial-guess))

#|
> (fixed-point cos 1.0)
0.7390822985224023
>
> (define (sqrt x)
    (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))
; no values returned
> (sqrt 2)
1.4142135623746899
> 
|#

