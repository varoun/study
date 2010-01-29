;;; fast-multiplication - recursive process
(define (fast-mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
	((even? b) 
	 (double (fast-mul a (halve b))))
	(else 
	 (+ a (fast-mul a (- b 1))))))

;; repl
; > (fast-mul 2 3)
; 6
; > (fast-mul 3 2)
; 6
; > (fast-mul 2 10)
; 20
; > 