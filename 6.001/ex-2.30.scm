;;;; Exercise 2.30 - square-tree

;; square-tree without using higher order procedures

(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree))
	(else (cons
	       (square-tree (car tree))
	       (square-tree (cdr tree))))))
#|
> (define (square n) (* n n))
; no values returned
> (square-tree (list 1
		     (list 2 (list 3 4) 5)
		     (list 6 7)))
'(1 (4 (9 16) 25) (36 49))
> 
|#


;; square-tree using map
(define (square-tree-m tree)
  (define (square x) (* x x))
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (square-tree-m subtree)
	     (square subtree)))
       tree))
#|
>  (square-tree-m (list 1
		     (list 2 (list 3 4) 5)
		     (list 6 7)))
'(1 (4 (9 16) 25) (36 49))
> 
|#
