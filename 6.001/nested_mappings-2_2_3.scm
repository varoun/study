;;;; Nested maps

;;; the enumerate procedure
(define (enumerate-interval low high)
  (if (> low high) 
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;; fold-right, aka accumulate
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (fold-right op initial (cdr sequence)))))

;;; filter
(define (filter predicate sequence)
  (cond ((null? sequence) '())
	((predicate (car sequence))
	 (cons (car sequence) 
	       (filter predicate (cdr sequence))))
	(else
	 (filter predicate (cdr sequence)))))
#|
> (map (lambda (i)
         (map (lambda(j) (list i j))
	      (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 6))
'(() ((2 1)) ((3 1) (3 2)) ((4 1) (4 2) (4 3)) ((5 1) (5 2) (5 3) (5 4)) ((6 1) (6 2) (6 3) (6 4) (6 5)))
>
|#

;;; flatmap
(define (flatmap proc sequence)
  (fold-right append '() (map proc sequence)))

#|
> (flatmap (lambda (i)
             (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 6))
'((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5))
> 
|#

;;; permutations of elements in a set
;;; We need a way to remove a particular element from a set, represented as a list
(define (remove item set)
  (filter (lambda (i) (not (= i item)))
	  set))
#|
> (remove 3 '(1 2 3 4 4 3 2 1))
'(1 2 4 4 2 1)
> 
|#

(define (permutations set)
  (if (null? set)
      (list '())
      (flatmap (lambda (i)
		 (map (lambda (p) (cons i p))
		      (permutations (remove i set))))
	       set)))
#|
> (permutations '(1 2 3))
'((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
> 
|#

		 
			