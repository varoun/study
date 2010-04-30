;;;; Infinite streams - Section 3.5.2

;;; stream-ref - 
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;; divisible?
(define (divisible? x y)
  (= (remainder x y) 0))
;;; Infinite streams of integers defined explicitly

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

#|
> (define integers (integers-starting-from 1))
; no values returned
> integers
#{Stream}
> 
> (stream-ref integers 10)
11
> 
|#

;;; infinite stream of primes, using the Sieve of Eratosthenes

(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter 
	   (lambda (x) (not (divisible? x (stream-car stream))))
	   (stream-cdr stream)))))
#|
> (define primes (sieve (integers-starting-from 2)))
; no values returned
> (stream-ref primes 50)
233
> 
|#

;;; Defining streams implicitly

#|
> (define ones (stream-cons 1 ones))
; no values returned
> ones
#{Stream}
> 
|#

;;; The add-streams procedure will enable us to generate 
;;; interesting streams from the stream of ones
(define (add-streams s1 s2)
  (stream-map + s1 s2))

#|
> (define int (stream-cons 1 (add-streams int ones)))
; no values returned
> int
#{Stream}
> (stream-ref int 10)
11
> 
> (define fibs (stream-cons 0 
			    (stream-cons 1
					 (add-streams (stream-cdr fibs)
						      fibs))))
; no values returned
> fibs
#{Stream}
> (stream-ref fibs 10)
55
> 
|#
