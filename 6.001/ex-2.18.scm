;;;; Exercise 2.18 - reverse a list

(define (reverse-list items)
  (define (iter source result)
    (if (null? source)
	result
	(iter (cdr source) (cons (car source) result))))
  (iter items '()))

#|
> (reverse-list (list 1 2 3 4 5))
'(5 4 3 2 1)
> 
|#
