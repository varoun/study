;;;; Section 3.5.3 - Formulating iterations as streams

;;; The square-root stream
(define (sqrt-improve guess x)
  (define (average x y) (/ (+ x y) 2))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map 
		      (lambda (guess) (sqrt-improve guess x))
		      guesses)))
  guesses)

;;;; The pi stream

;;; We need partial-sums
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Ex 3.55
(define (partial-sums s)
  (define ps (cons-stream (stream-car s)
			  (add-streams ps (stream-cdr s))))
  ps)

;;; test with the stream of integers
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))

#|
1 ]=> (define p1 (partial-sums integers))
;Value: p1
1 ]=> p1
;Value 19: (1 . #[promise 20])
1 ]=> (stream-ref p1 1)
;Value: 3
1 ]=> (stream-ref p1 2)
;Value: 6
1 ]=> (stream-ref p1 3)
;Value: 10
1 ]=> (stream-ref p1 4)
;Value: 15
1 ]=> 
|#

;;; The PI stream
;; scale-stream
(define (scale-stream stream factor)
  (stream-map (lambda (n) (* n factor)) stream))

;; pi-summands
(define (pi-summands n)
  (cons-stream (/ 1.0 n) ; mapping - results in alternating series
	       (stream-map - (pi-summands (+ n 2)))))

;; the pi-stream
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
#|
1 ]=> (stream-ref pi-stream 10)
;Value: 3.232315809405594
1 ]=> 
|#

;; The pi-stream above converges rather slowly!

;;; Accelerated streams

;;; Acceleration by Euler Transform
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))
#|
1 ]=> (stream-ref (euler-transform pi-stream) 10)
;Value: 3.1417360992606667
1 ]=> 
|#

;;; Accelerating an accelerated sequence
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
#|
1 ]=> (stream-ref (accelerated-sequence euler-transform pi-stream) 8)
;Value: 3.1415926535897953
1 ]=> 
|#
