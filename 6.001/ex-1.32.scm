;;; Exercise 1.32 -
;;; The accumulate procedure

;; recursive process
(define (accumulate-r combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-r combiner
			      null-value
			      term
			      (next a)
			      next
			      b))))

;; iterative process
(define (accumulate-i combiner null-value term a next b)
  (define (iter index result)
    (if (> index b)
	result
	(iter (next index) (combiner result (term index)))))
  (iter a null-value))

#|
> (define (identity x) x)
; no values returned
> (define (inc x) (+ x 1))
; no values returned
> (accumulate-r + 0 identity 0 inc 10)
55
> (accumulate-r * 1 identity 1 inc 4)
24
> (accumulate-i + 0 identity 0 inc 10)
55
> (accumulate-i * 1 identity 1 inc 4)
24
>
|#
