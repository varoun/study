;;;; Exercise 2.12 - intervals specified as a center and a tolerance

;; The original constructors and selectors

(define (make-interval low high)
  (cons low high))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;; specifying itervals as a tolerance width around a center value

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; specifying intervals as a center value and a tolerance percent

(define (make-center-percent c p)
  (let ((w (* (/ p 100) c)))
    (make-center-width c w)))

(define (percent i)
  (let ((c (center i))
	(w (width i)))
    (* (/ w c) 100)))

#|
> (define r1 (make-center-percent 6.8 10))
; no values returned
> (define r2 (make-center-percent 4.7 5))
; no values returned
>
> (percent r1)
9.999999999999996
> (percent r2)
5.000000000000006
> 
|#

;;;; Exercise 2.13
(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
	(p2 (* (lower-bound a) (upper-bound b)))
	(p3 (* (upper-bound a) (lower-bound b)))
	(p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

#|
> (define i1 (make-center-percent 3 .5))
; no values returned
> (define i2 (make-center-percent 5 .4))
; no values returned
> 
> (define i-mul (mul-interval i1 i2))
; no values returned
> (percent i1)
0.5000000000000042
> (percent i2)
0.39999999999999153
> (percent i-mul)
0.8999820003599855
> (+ (percent i1) (percent i2))
0.8999999999999957
> 
|#

	 