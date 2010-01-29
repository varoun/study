;;;; Cube root procedure

(define (cube-root n)
  (define (cube x) (* x x x))
  (define (square x) (* x x))
  (define (improve guess)
    (/ (+ (/ n (square guess))
	  (* 2 guess))
       3))
  (define (good-enuf? guess)
    (< (abs (- (cube guess) n)) 0.0001))
  (define (cube-iter guess)
    (if (good-enuf? guess)
	guess
	(cube-iter (improve guess))))
  (cube-iter 1.0))

;; repl
; > (cube-root 27)
; 3.0000005410641766
; > 
