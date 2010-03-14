;;;; Section 2.2.3 - Sequences as conventional interfaces

;;; filter
(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence)) 
	 (cons (car sequence)
	       (filter predicate (cdr sequence) )))
	(else
	 (filter predicate (cdr sequence)))))
#|
> (filter odd? '(1 2 3 4 5))
'(1 3 5)
> 
|#

;;; accumulate
(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial
      (op (car sequence) ; op is a procedure of two args
	  (accumulate op initial (cdr sequence)))))
#|
> (accumulate + 0 '(1 2 3 4 5))
15
> 
|#

;;; enumerate-interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

#|
> (enumerate-interval 1 10)
'(1 2 3 4 5 6 7 8 9 10)
> (define (square x) (* x x))
; no values returned
> (accumulate + 0
	      (map square
		   (filter odd?
			   (enumerate-interval 1 10))))
165
>  (accumulate cons '()
	       (map square
		    (filter odd?
			    (enumerate-interval 1 10))))
'(1 9 25 49 81)
>  
|#
