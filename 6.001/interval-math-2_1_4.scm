;;;; Section 2.1.4 - Interval Arithmetic

;;; Abstract data - constructors and selectors

(define (make-interval low high)
  (cons low high))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))


;; addition

(define (add-interval a b)
  (make-interval 
   (+ (lower-bound a) (lower-bound b))
   (+ (upper-bound a) (upper-bound b))))

;; multiplication

(define (mul-interval a b)
  (let ((p1 (* (lower-bound a) (lower-bound b)))
	(p2 (* (lower-bound a) (upper-bound b)))
	(p3 (* (upper-bound a) (lower-bound b)))
	(p4 (* (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;; division
(define (div-interval a b)
  (mul-interval a
		(make-interval (/ 1.0 (lower-bound b)) 
			       (/ 1.0 (upper-bound b)))))
;;; subtraction
;; Ex - 2.8
(define (sub-interval a b)
  (make-interval 
   (- (lower-bound a) (upper-bound b))
   (- (upper-bound a) (lower-bound b))))

;;; interval width
;; Ex - 2.9
(define (width interval)
  (define (average x y) (/ (+ x y) 2))
  (average (lower-bound interval) (upper-bound interval)))

#| repl
> (define i1 (make-interval 6.12 7.48))
; no values returned
> (define i2 (make-interval 4.465 4.935))
; no values returned
> (width i1)
6.800000000000001
> (width i2)
4.699999999999999
> (define i-sum (add-interval i1 i2))
; no values returned
> (define i-sub (sub-interval i1 i2))
; no values returned
> (width i-sum)
11.5 ; the width of the result is the sum of the widths of the arguments
> (width i-sub)
2.1000000000000005 ; width of the result is the difference of the widths of the args
> (define i-mul (mul-interval i1 i2))
; no values returned
> (define i-div (div-interval i1 i2))
; no values returned
> (width i-mul)
32.1198 ; width of the result is not a function of the widths of the args
> (width i-div)
1.4576867701167815 ; the width of the result is not a function of the widths of the args
> 
|#

;;; Improved div-interval - signals an error if the intervals span zero
;;; Ex - 2.10
;; the spans-zero? predicate
(define (spans-zero? interval)
  (and (negative? (lower-bound interval))
       (positive? (upper-bound interval))))

;; improved div-interval
(define (div-interval-new a b)
  (if (spans-zero? b)
      (error "Dividend spans zero" b)
      (mul-interval a
		    (make-interval (/ 1.0 (lower-bound b))
				   (/ 1.0 (upper-bound b))))))
#|
> (define i3 (make-interval -3.14 2.17))
; no values returned
> (div-interval i1 i3)
'(-2.3821656050955413 . 3.447004608294931)
> (div-interval-new i1 i3)

Error: too many arguments to FORMAT
       "Dividend spans zero"
       ((-3.14 . 2.17))
       (&error)
1> 
|#

