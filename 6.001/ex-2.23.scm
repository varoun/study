;;;; Exercise 2.23 - for-each

(define (for-each proc items)
  (cond ((null? items) #t)
	(else
	 (proc (car items))
	 (for-each proc (cdr items)))))

#|
> (for-each (lambda (x) (newline) (display x))
	    (list 1 2 3 4 5))

1
2
3
4
5#t
> 
|#
