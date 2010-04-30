;;;; Exercise 3.19
;;;; Finding out if a list has a cycle in constant space

(define (contains-cycle? lst)
  (define (iter ptr1 ptr2)
    (cond ((eq? ptr1 ptr2) #t)
	  ((or (null? ptr1) (null? ptr2)) #f) ; maybe wrong ??
	  (else (iter (cdr ptr1) (cddr ptr2)))))
  (if (and (pair? lst) (pair? (cdr lst)))
      (iter lst (cddr lst))
      #f))

#|
> 
> (contains-cycle? a) ; this is the list from ex-3.18
#t
> 
|#

