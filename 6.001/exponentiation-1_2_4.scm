;;; Exponentiation
;;; Section 1.2.4

;; linear-recursive process
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

;; iterative process
(define (expt-iter b n)
  (define (iter product counter)
    (if (= counter 0)
	product
	(iter (* b product) (- counter 1))))
  (iter 1 n))

;; fast-expt - recursive process with log growth
(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))