;;; Two procedures to test for primality of numbers

;; order of growth of square-root of n

(define (prime? n)
  (define (square x) (* x x))
  (define (divides? a b)
    (= (remainder a b) 0))
  (define (find-divisor a)
    (cond ((> (square a) n) n)
	  ((divides? n a) a)
	  (else (find-divisor (+ a 1)))))
  (define (smallest-divisor)
    (find-divisor 2))
  (= n (smallest-divisor)))

;; repl
; > (prime? 12)
; #f
; > (prime? 13)
; #t
; >

;; fast-prime - order of growth of log n

(define (fast-prime? n times)
  (define (square x) (* x x))
  (define (expmod base exp)
    (cond ((= exp 0) 1)
	  ((even? exp)
	   (remainder (square (expmod base (/ exp 2) n)) n))
	  (else 
	   (remainder (* base (expmod base (- exp 1) n)) n))))
  (define (fermats-test)
    (define (try a)
      (= (expmod a n) a))
    (try (+ 1 (random (- n 1)))))
  (cond ((= times 0) 'true)
	((fermats-test) (fast-prime? n (- times 1)))
	(else 'false)))

;; scheme48 does not include the random procedure

	   