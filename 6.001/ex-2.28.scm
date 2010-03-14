;;;; Exercise 2.28 - The fringe procedure

(define (fringe tree)
  (cond ((null? tree) '())
	((not (pair? (car tree)))
	 (cons (car tree) (fringe (cdr tree)))) 
	(else
	 (append (fringe (car tree)) (fringe (cdr tree))))))

#|
> (fringe '((1 2) (3 4)))
'(1 2 3 4)
> (fringe  '(1 2 (3 4) 5 (6 (7 8) 9) 10))
'(1 2 3 4 5 6 7 8 9 10)
> (fringe '(1 2 3 4))
'(1 2 3 4)
> (fringe '(1 (2 (3 (4 (5 6))))))
'(1 2 3 4 5 6)
> 
|#

;; a simpler version of fringe
(define (fringe-new tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append
	       (fringe-new (car tree))
	       (fringe-new (cdr tree))))))
#|
> (fringe-new '(1 (2 (3 (4 (5 6))))))
'(1 2 3 4 5 6)
> 
#|
