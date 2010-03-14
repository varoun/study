;;;; Exercise 2.35 - counting the leaves of a tree using accumulation

;;; the accumulate procedure
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;;; enumerate-leaves
(define (enumerate-leaves tree)
  (cond ((null? tree) '())
	((not (pair? tree))
	 (list tree))
	(else (append (enumerate-leaves (car tree))
		      (enumerate-leaves (cdr tree))))))
;;; count-leaves
(define (count-leaves tree)
  (accumulate +
	      0
	      (map (lambda (item) 1)
		   (enumerate-leaves tree))))
#|
> 
> (count-leaves '(1 (2 3 (4 5) 6) 7))
7
|#

		     
		    