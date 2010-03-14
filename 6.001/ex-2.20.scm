;;;; Exercise 2.20 - same-parity

(define (same-parity initial . rest)
  (let ((parity? (if (even? initial) even? odd?)))
    (define (recur items)
      (cond ((null? items) items)
	    ((parity? (car items))
	     (cons (car items) (recur (cdr items))))
	    (else (recur (cdr items)))))
    (recur (cons initial rest))))


#|
> (same-parity 1 2 3 4 5 6 7)
'(1 3 5 7)
> (same-parity 2 3 4 5 6 7)
'(2 4 6)
> 
|#

		   