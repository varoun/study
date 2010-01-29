;;; Procedures as returned values
;;; Fixed point used with average damping and newtons method

;; Tolerance, used with fixed point
(define tolerance 0.000001)

;; dx, used with deriv
(define dx 0.000001)

;; fixed-point search
(define (fixed-point f g)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (search guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (search next))))
  (search g))

;; average-damp
(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

#| repl 
> (define (square n) (* n n))
; no values returned
> ((average-damp square) 10)
55
>
> (define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
		 1.0))
; no values returned
> (sqrt 2.0)
1.414213562373095
> 
|#

;; The deriv procedure, used with newtons transform
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

;; newtons transform
(define (newtons-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x)))))

(define (newtons-method f x)
  (fixed-point (newtons-transform f) x))

#| repl
> (define (cube x) (* x x x))
; no values returned
> ((deriv cube) 5)
75.00001501625775
>
> (define (sqrt-newtons x)
    (newtons-method (lambda (y) (- (square y) x)) 1.0))
; no values returned
> (sqrt-newtons 2.0)
1.4142135623730951
> 
|#

;; fixed-point of a transformed function (by average-damp or newtons-transform)


(define (fixed-point-transform f transform guess)
  (fixed-point (transform f) guess))

#| repl
> (define (sqrt-avedamp x)
    (fixed-point-transform (lambda (y) (/ x y))
			   average-damp
			   1.0))
; no values returned
> (sqrt-avedamp 2.0)
1.414213562373095
> (define (sqrt-newtons-trans x)
    (fixed-point-transform (lambda (y) (- (square y) x))
			   newtons-transform
			   1.0))
; no values returned
> (sqrt-avedamp 2.0)
1.414213562373095
> 
|#
