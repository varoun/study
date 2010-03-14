;;;; Exercise 2.40 - unique-pairs

;;; enumerate-interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;; fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;; flatmap
(define (flatmap proc sequence)
  (fold-right append '() (map proc sequence)))

;;; unique-pairs
(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))
#|
> (unique-pairs 6)
'((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5))
> 
|#
