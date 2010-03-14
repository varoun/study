;;;; Execise 2.54.scm
;;;; The equal? procedure

(define (my-equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (pair? a) (pair? b))
	 (and (my-equal? (car a) (car b))
	      (my-equal? (cdr a) (cdr b))))
	(else #f)))

#|
> (my-equal? '(a b c) '(a b c))
#t
> (my-equal? '(this is a list) '(this (is a) list))
#f
> (my-equal? '(this (is a) list) '(this (is a) list))
#t
> (my-equal? 'a 'a)
#t
> (my-equal? 'a 'b)
#f
> (my-equal? '(this is a list) 'a-symbol)
#f
> 
|#

	      