;;; Half interval method - Chapter 1.3.3

(define (search f neg-point pos-point)
  (define (average x y) (/ (+ x y) 2))
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	mid-point
	(let ((test-value (f mid-point)))
	  (cond ((negative? test-value) (search f mid-point pos-point))
		((positive? test-value) (search f neg-point mid-point))
		(else mid-point))))))

(define (half-interval f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (positive? a-value) (negative? b-value))
	   (search f b a))
	  (else (error "Values are not of opposite sign" )))))

#|
> (half-interval sin 2.0 4.0)
3.14111328125
>
|#
