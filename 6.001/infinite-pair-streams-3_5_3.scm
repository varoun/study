;;;; infinite pairs of streams
;;; implementation: mit-scheme
;;; We need a way to generate an infinite stream of integers
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams integers ones)))


;;; interleaving streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

;;; generating pairs of streams such that i<=j
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave 
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;;; test procedure to print the first n elements of a stream
(define (print-n stream n)
  (define (loop counter)
    (if (= counter n)
	'done
	(begin
	  (display (stream-ref stream counter))
	  (newline)
	  (loop (+ counter 1)))))
  (loop 0))

#|
1 ]=> (define p (pairs integers integers))
;Value: p
1 ]=> (print-n p 15)
(1 1)
(1 2)
(2 2)
(1 3)
(2 3)
(1 4)
(3 3)
(1 5)
(2 4)
(1 6)
(3 4)
(1 7)
(2 5)
(1 8)
(4 4)
;Value: done

1 ]=> 
|#
