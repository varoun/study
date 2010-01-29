;;; Exercise 1.31 - Product 

;; recursive process
(define (prod-rec term a next b)
  (if (> a b) 
      1
      (* (term a)
	 (prod-rec term (next a) next b))))

;; iterative process
(define (prod-iter term a next b)
  (define (iter a prod)
    (if (> a b)
	prod
	(iter (next a) (* prod (term a)))))
  (iter a 1))

;; factorial using product
(define (fact-rec n)
  (prod-rec (lambda (x) x)
	    1
	    (lambda (x) (+ x 1))
	    n))
(define (fact-iter n)
  (prod-iter (lambda (x) x)
	     1
	     (lambda (x) (+ x 1))
	     n))

;; pi over 4
(define (pi-over-4 n)
  (define (square x) (* x x))
  (define (current-term-val index)
    (/ (* index (+ index 2.0)) ; use reals instead of integers, 2.0 instead of 2
       (square (+ index 1.0)))) ; same as above - w/o reals, computing for large n fails
  (define (add-2 index)
    (+ index 2))
  (prod-iter current-term-val 2 add-2 n))

#|
> (fact-rec 4)
24
> (fact-rec 4)
24
> (* 4.0 (pi-over-4 1000)) ; before using reals s noted in the comment
#{NaN}
> (* 4.0 (pi-over-4 1000)) ; after changing to reals
3.1431607055322712
> (* 4.0 (pi-over-4 1000000))
3.141594224383365
> 
|#

