;;; Exercise 1.30 - iterative higher order procedure sum

(define (sum term a next b)
  (define (iter a result)
    (if (> a b) 
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

#|
> (define (inc n) (+ n 1))
; no values returned
> (define (identity n) n)
; no values returned
> (define (cube n) (* n n n))
; no values returned
> (sum cube 1 inc 100)
25502500
> (sum cube 1 inc 10)
3025
> 
|#

