;;;; Exercise 2.41

;;; enumerate-interval
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;; filter 
(define (filter pred seq)
  (cond ((null? seq) '())
	((pred (car seq))
	 (cons (car seq) (filter pred (cdr seq))))
	(else (filter pred (cdr seq)))))


;;; fold-right
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;; flatmap
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))


;;; creating all triples for a given n
(define (triples n)
  (flatmap (lambda (rest)
	     (map (lambda (i) (cons i rest ))
		  (enumerate-interval 1 n)))
	   (flatmap (lambda (j)
		      (map (lambda (k) (list j k))
			   (enumerate-interval 1 n)))
		    (enumerate-interval 1 n))))

;;; triple-sum
(define (triple-sum num sum)
  (filter (lambda (triple)
	    (= (+ (car triple) (cadr triple) (caddr triple)) sum))
	  (triples num)))
#|
> (triple-sum 3 4)
'((2 1 1) (1 1 2) (1 2 1))
> (triple-sum 3 3)
'((1 1 1))
> (triple-sum 3 6)
'((3 1 2) (2 1 3) (3 2 1) (2 2 2) (1 2 3) (2 3 1) (1 3 2))
> 
|#

	 