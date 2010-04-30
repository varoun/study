;;;; Exercise 3.52

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      stream-null
      (stream-cons low 
		   (stream-enumerate-interval (+ low 1) high))))

(define (accum x)
  (set! sum (+ x sum))
  sum)

#|
> (define sum 0)
; no values returned
> (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; no values returned
> (display sum)
0#{Unspecific}
> (define y (stream-filter even? seq))
; no values returned
> (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; no values returned
> (display sum)
0#{Unspecific}
> (stream-ref y 7)
136
> (display-stream z)

10
15
45
55
105
120
190
210
Error: attempt to take stream-cdr of null stream
       (&error)
1>
|#
