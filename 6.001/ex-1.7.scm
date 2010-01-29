;; Exercise 1.7
;; The good-enuf? procedure checks to see how the guess changes from one 
;; iteration to the next and stops when the change is a small fraction of the 
;; guess

;; average - 
(define (average x y)
  (/ (+ x y) 2))

;; improve
(define (improve guess x)
  (average guess (/ x guess)))

;; good-enuf?
(define (good-enuf? old-guess new-guess)
  (< (/ (abs (- old-guess new-guess)) old-guess) 0.001))

;; sqrt-iter
(define (sqrt-iter old-guess new-guess x)
  (if (good-enuf? old-guess new-guess)
      old-guess
      (sqrt-iter new-guess (improve new-guess x) x)))

;; sqrt
(define (sqrt n)
  (sqrt-iter 1.0 (improve 1.0 n) n))

;; output for large and small numbers
;> (square (sqrt 2.0e16))
;20000020338897812.0
;> (square (sqrt 2.0e-16))
;2.0000020338897814e-16
;> (square (sqrt 2.0e-40))
;2.0018957739794304e-40
;> (square (sqrt 2.0e40))
;2.0018957739794303e40
;> 
