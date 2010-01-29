;;; The sum higher order procedure
;;; Section 1.3.1

;; sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

;; sum-int
(define (sum-int a b)
  (sum
   (lambda (x) x)
   a
   (lambda (n) (+ n 1))
   b))

;; sum-cube
(define (sum-cube a b)
  (sum
   (lambda (n) (* n n n))
   a
   (lambda (x) (+ x 1))
   b))

;; pi over 8
(define (pi-over-8 a b)
  (sum
   (lambda (n) (/ 1.0 (* n (+ n 2))))
   a
   (lambda (x) (+ x 4))
   b))

;; use sum as a component for rough integration
(define (integral f a b dx)
  (* 
   (sum
    f
    (+ a (/ dx 2))
    (lambda (n) (+ n dx))
    b)
   dx))


#| repl
> (sum-int 1 2)
3
> (sum-int 1 10)
55
> 
> (sum-cube 1 10)
3025
> (* 8 (pi-over-8 1 1000))
3.139592655589783
> 
> 
> (integral (lambda (x) (* x x x))
	    0 1 0.01)
0.24998750000000042
> 
|#

