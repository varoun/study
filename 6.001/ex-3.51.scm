;;;; Exercise 3.51 
;;;; ,open srfi-40

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      stream-null
      (stream-cons low (stream-enumerate-interval (+ low 1) high))))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (show x)
  (display-line x)
  x)

#|
> (define x (stream-map show (stream-enumerate-interval 0 10)))
; no values returned
> x
#{Stream}
> (stream-ref x 5)

0
1
2
3
4
55
> (stream-ref x 7)

6
77
> 
|#

;; Note:  SRFI-40 provides even streams,both the car and cdr are 
;; delayed. If we had odd streams, as dscribed in the book, the 
;; result above would have been different. Specifically, when 
;; defining x, the first element in the stream would have been
;; displayed ?
