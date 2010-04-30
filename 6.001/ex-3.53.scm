;;;; Exercise 3.53

(define (add-streams s1 s2)
  (stream-map + s1 s2))

#|
1 ]=> (define s (cons-stream 1 (add-streams s s)))
;Value: s
1 ]=> (stream-ref s 0)
;Value: 1
1 ]=> (stream-ref s 1)
;Value: 2
1 ]=> (stream-ref s 2)
;Value: 4
1 ]=> (stream-ref s 3)
;Value: 8
1 ]=> 
|#
