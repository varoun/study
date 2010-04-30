;;;; Exercise 3.18 - Program to check if a list is circular

(define (circular-list? lst)
  (define loop 
    (let ((seen '()))
      (lambda (items)
	(cond ((null? items) #f)
	      ((memq items seen)
	       #t)
	      (else 
	       (set! seen (cons items seen))
	       (loop (cdr items)))))))
  (loop lst))

;;; Aux procedures to create a list cycle
(define (last-pair x)
  (if (null? (cdr x)) 
      x
      (last-pair (cdr x))))

(define (make-cycle lst)
  (set-cdr! (last-pair lst) lst))
  
#|
> (define a (list 'a 'b 'c 'd))
; no values returned
> a
'(a b c d)
> (circular-list? a)
#f
> (make-cycle a)
#{Unspecific}
> (length a) ; this does infinite cdring
  C-c C-c
Condition: (&interrupt keyboard)
1> ,reset

Top level
> (circular-list? a)
#t
>
|#
