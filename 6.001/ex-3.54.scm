;;;; Exercise 3.54

;;; add-streams
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;;; mul-streams

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; stream-of-integers
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;; factorials

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

#|
1 ]=> (stream-ref factorials 0)
;Value: 1
1 ]=> (stream-ref factorials 1)
;Value: 1
1 ]=> (stream-ref factorials 2)
;Value: 2
1 ]=> (stream-ref factorials 3)
;Value: 6
1 ]=> (stream-ref factorials 4)
;Value: 24
1 ]=> 
|#
