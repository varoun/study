;;; Exercise 1.44 -- Smoothing a function

;; dx
(define dx 0.00001)

;; compose
(define (compose f g)
  (lambda (x) (f (g x))))

;; repeated
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;; smooth
(define (smooth f)
  (define (average n1 n2 n3) (/ (+ n1 n2 n3) 3))
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

;; n-fold-smooth
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

#|
> ((smooth sin) 3)
0.1411200080551632
> ((smooth (smooth sin)) 3)
0.1411200080504592
> ((n-fold-smooth sin 1) 3)
0.1411200080551632
>
> ((n-fold-smooth sin 2) 3)
0.1411200080504592
> 
|#
