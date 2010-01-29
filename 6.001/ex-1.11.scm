;;; Recursive and iterative implementations of:
;;; f(n) = n if n < 3 and f(n-1) + 2f(n-2) +3f(n-3) otherwise

;; f-rec
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
	 (* 2 (f-rec (- n 2)))
	 (* 3 (f-rec (- n 3))))))

;; f-iter
(define (f-iter n)
  (define (iter n0 n1 n2 count)
    (if (= count n) 
	n0
	(iter n1 n2 (+ n2 (* 2 n1) (* 3 n0)) (+ count 1))))
  (iter 0 1 2 0))

;; repl
; > (f-rec 30)
; 61354575194
; > (f-iter 30)
; 61354575194
; > 
