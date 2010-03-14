;;;; Exercise 2.27 - deep-reverse

;; the reverse procedure

(define (reverse items)
  (define (iter l result)
    (if (null? l)
	result
	(iter (cdr l) (cons (car l) result))))
  (iter items '()))

#|
> (reverse '(1 2 3 4))
'(4 3 2 1)
> (reverse '((1 2) (3 4)))
'((3 4) (1 2))
> (reverse '((1 2) 3 4))
'(4 3 (1 2))
> 
|#

;; deep-reverse
(define (deep-reverse tree)
  (define (iter t result)
    (cond ((null? t) result)
	  ((not (pair? (car t)))
	   (iter (cdr t) (cons (car t) result)))
	  (else 
	   (iter (cdr t) (cons (deep-reverse (car t)) result)))))
  (iter tree '()))

#|
> (deep-reverse '((1 2) (3 4)))
'((4 3) (2 1))
> (deep-reverse '(1 2 (3 4) 5 (6 (7 8) 9) 10))
'(10 (9 (8 7) 6) 5 (4 3) 2 1)
> 
|#


