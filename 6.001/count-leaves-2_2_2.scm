;;;; Section 2.2.2 - counting the leaves of a tree

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else 
	 (+ (count-leaves (car tree))
	    (count-leaves (cdr tree))))))

#|
> (define x (cons (list 1 2) (list 3 4)))
; no values returned
> x
'((1 2) 3 4)
> (length x)
3
> (count-leaves x)
4
> (length (list x x))
2
> (count-leaves (list x x))
8
> (list x x)
'(((1 2) 3 4) ((1 2) 3 4))
> 
|#


	