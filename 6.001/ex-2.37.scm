;;;; Exercise 2.37 - Basic matrix and vector operations

;;; We need accumulate and accumulate-n
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op initial (map car seqs))
	    (accumulate-n op initial (map cdr seqs)))))

;;; dot-product
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
#|
> (dot-product '(1 2 3) '(4 5 6))
32
> 
|#

;;; matrix * vector
(define (matrix-*-vector m v)
  (map (lambda (row) 
	 (dot-product row v))
       m))
#|
> (matrix-*-vector '((1 2) (3 4)) '(2 3))
'(8 18)
> 
|#

;;; transpose 
(define (transpose mat)
  (accumulate-n cons '() mat))
#|
> (transpose '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
'((1 5 9) (2 6 10) (3 7 11) (4 8 12))
> 
|#

;;; matrix * matrix
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
	   (matrix-*-vector cols v))
	 m)))
#|
> (matrix-*-matrix '((1 2 3) (4 5 6) (7 8 9))
		   '((1 2) (3 4) (5 6)))
'((22 28) (49 64) (76 100))
> (matrix-*-matrix '((1 2 3 4) (5 6 7 8) (9 10 11 12))
		   '((1 2 3 4 5) (6 7 8 9 10) (11 12 13 14 15) (16 17 18 19 20)))
'((110 120 130 140 150) (246 272 298 324 350) (382 424 466 508 550))
> 
|#
