;;; fib with log growth

(define (fib-iter a b p q count)
  (define (square x) (* x x))
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a b
		   (