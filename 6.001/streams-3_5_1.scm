;;;; Streams -- Section 3.5.1
;;;; SRFI-40 provides stream-cons, stream-car, stream-null, etc

;; stream-enumerate-interval

(define (stream-enumerate-interval low high)
  (if (> low high)
      stream-null
      (stream-cons low
                   (stream-enumerate-interval (+ low 1) high))))
#|
> (define a (stream-enumerate-interval 1 10))
; no values returned
> a
#{Stream}
> (stream-car a)
1
> (stream-car (stream-cdr a))
2
>
|#

;;; displaying a stream
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

;;; stream-ref
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

