;;;; Exercise 2.31 - tree-map

(define (tree-map proc tree)
  (map (lambda (subtree)
	 (if (pair? subtree)
	     (tree-map proc subtree)
	     (proc subtree)))
       tree))

#|
>  (tree-map square (list 1
			 (list 2 (list 3 4) 5)
			 (list 6 7)))
'(1 (4 (9 16) 25) (36 49))
> 
|#
