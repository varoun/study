;;; Exercise 1.41 - 

;; the double procedure - takes a procedure that represents a function, and 
;; returns a procedure applies the original procedure twice

(define (double f)
  (lambda (x) (f (f x))))

#|
> ((double inc) 1)
3
> (((double (double double)) inc) 5)
21
> 
|#
