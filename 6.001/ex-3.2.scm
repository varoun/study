;;;; Exercise 3.2 - MAKE-MONITORED

(define (make-monitored f)
  (let ((calls 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) calls)
	    ((eq? m 'reset-count) 
	     (set! calls 0)
	     calls)
	    (else (set! calls (+ calls 1))
		  (f m))))))
#|
> (define S (make-monitored sqrt))
; no values returned
> S
#{Procedure 8608 (unnamed in make-monitored)}
> (S 100)
10.0
> (S 'how-many-calls?)
1
> (S 4)
2.0
> (S 'how-many-calls)

Error: vm-exception
       #f
       (sqrt 'how-many-calls)
1> ,reset

Top level
> (S 'how-many-calls?)
3
> (S 'reset-count)
0
> (S 'how-many-calls?)
0
> 
|#
