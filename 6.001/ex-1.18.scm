;;; fast multiplication - iterative process with log growth

(define (fast-mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (iter multiplicand multiplier a)
    (cond ((= multiplier 0) a)
	  ((even? multiplier)
	   (iter (double multiplicand) (halve multiplier) a))
	  (else 
	   (iter multiplicand (- multiplier 1) (+ a multiplicand)))))
  (iter a b 0))

;; repl
; > (fast-mul 2 3)
; 6
; > (fast-mul 3 2)
; 6
; > (fast-mul 2 10)
; 20
; > (fast-mul 10 2)
; 20
; >

 