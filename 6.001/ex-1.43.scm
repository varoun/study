;;; Exercise 1.43 - 
;;; The repeated procedure that applied returns a procedure that applies f n times

;; compose
(define (compose f g)
  (lambda (x) (f (g x))))

;; repeated
(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose (lambda (x) (f x)) (repeated f (- n 1)))))

#|
> ((repeated square 2) 5)
625
> 
> ((repeated (lambda (x) (+ x 1)) 1) 1)
2
> ((repeated (lambda (x) (+ x 1)) 2) 1)
3
> ((repeated (lambda (x) (+ x 1)) 10) 1)
11
> 
|#

;; better code ?
(define (repeat f n)
  (if (= n 1) 
      f
      (compose f (repeat f (- n 1)))))

#|
> ((repeat (lambda (x) (+ x 1)) 10) 1)
11
> ((repeat (lambda (x) (+ x 1)) 1) 1)
2
> |#
