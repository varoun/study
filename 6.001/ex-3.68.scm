;;;; Implementation: mit-scheme
;;;; Exercise 3.68

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (interleave 
   (stream-map (lambda (x) (list (stream-car s) x))
	       t)
   (pairs (stream-cdr s) (stream-cdr t))))

;;; testing this out
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))


#|
1 ]=> (define p (pairs integers integers))
;Aborting!: maximum recursion depth exceeded
1 ]=>
|#

;;; Tracing reveals some details
#|
1 ]=> (trace pairs)
;Unspecified return value
1 ]=> (define p (pairs integers integers))

[Entering #[compound-procedure 11 pairs]
    Args: (1 . #[promise 12])
          (1 . #[promise 12])]
[Entering #[compound-procedure 11 pairs]
    Args: (2 . #[promise 13])
          (2 . #[promise 13])]
[Entering #[compound-procedure 11 pairs]
    Args: (3 . #[promise 14])
          (3 . #[promise 14])]
[Entering #[compound-procedure 11 pairs]
    Args: (4 . #[promise 15])
          (4 . #[promise 15])]
[Entering #[compound-procedure 11 pairs]
    Args: (5 . #[promise 16])
          (5 . #[promise 16])]
|#


