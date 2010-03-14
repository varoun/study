;;;; mapping over trees
;;; scale-tree

(define (scale-tree tree factor)
  (cond ((null? tree) '())
	((not (pair? tree)) (* factor tree))
	(else (cons (scale-tree (car tree) factor) 
		    (scale-tree (cdr tree) factor)))))

;; scale-tree using map
(define (scale-tree-m tree factor)
  (map (lambda (subtree)
	 (if (pair? subtree) 
	     (scale-tree-m subtree factor)
	     (* factor subtree)))
       tree))

#|
> (scale-tree '(1 (2 3) 4 (5 (6 7)) 8) 10)
'(10 (20 30) 40 (50 (60 70)) 80)
> (scale-tree-m '(1 (2 3) 4 (5 (6 7)) 8) 10)
'(10 (20 30) 40 (50 (60 70)) 80)
> 
|#
