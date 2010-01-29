;;; Exercise 1.33
;;; the filtered-accumulate procedure

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter index result)
    (cond ((> index b) result)
	  ((filter index) (iter (next index) 
				(combiner result (term index))))
	  (else (iter (next index) result))))
  (iter a null-value))

;; simple prime? procedure

(define (prime? n)
  (define (square i) (* i i))
  (define (divides? a b) (= (remainder b a) 0))
  (define (smallest-factor x)
    (define (iter num)
      (cond ((> (square num) x) x)
	    ((divides? num x) num)
	    (else (iter (+ num 1)))))
    (iter 2))
  (= (smallest-factor n) n))


;; sum of squares of prime numbers
(define (sum-square-prime a b)
  (define (square n) (* n n))
  (define (inc n) (+ n 1))
  (filtered-accumulate prime? + 0 square a inc b))

#|
repl -
> (sum-square-prime 2 5)
38
>
|#
