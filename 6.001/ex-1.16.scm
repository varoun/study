;;; Iterative process with log growth for exponentiation

(define (fast-expt b n)
  (define (square x) (* x x))
  (define (iter base exponent a)
    (cond ((= exponent 0) a)
	  ((even? exponent) 
	   (iter (square base) (/ exponent 2) a))
	  (else (iter base (- exponent 1) (* base a)))))
  (iter b n 1))

; > (fast-expt 2 10)
; 1024
; >
