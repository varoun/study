;;;; Exercise 2.38 -- fold-right and fold-left

;;; fold-right aka accumulate
(define (fold-right op initial sequence)
  (if (null? sequence) 
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;; fold-left
(define (fold-left op initial sequence)
  (define (iter result seq)
    (if (null? seq)
	result
	(iter (op result (car seq))
	      (cdr seq))))
  (iter initial sequence))

#|
> (fold-right / 1 '(1 2 3))
3/2
> (fold-left / 1 '(1 2 3))
1/6
> (fold-right list '() '(1 2 3))
'(1 (2 (3 ())))
> (fold-left list '() '(1 2 3))
'(((() 1) 2) 3)
> 
|#

;; for fold-right and fold-left to give the same result,
;; op should be associative, (op x (op y z)) should equal (op (op x y) z)
;; this should not be confused with commutativeness

