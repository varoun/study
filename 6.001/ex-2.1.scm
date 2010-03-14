;;; Exercise 2.1 - 
;;; A better version of rational numbers that can handle positive and negative numbers
;;; This also reduces to lowest terms

;; gcd - used to reduce a rational number to lowest terms during construction

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

;; constructor
(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (or (negative? n) (negative? d))
	(cons (- (/ (abs n) g)) (/ (abs d) g))
	(cons (/ n g) (/ d g)))))

;; selectors

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

#|
> (make-rat 18 24)
'(3 . 4)
> (make-rat -18 24)
'(-3 . 4)
> (make-rat 18 -24)
'(-3 . 4)
> 
|#

