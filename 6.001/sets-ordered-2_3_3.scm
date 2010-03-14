;;;; Sets of numbers expressed as a list of ordered elements

;;; element-of-set
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-set? x (cdr set)))))
#|
> (element-of-set? 3 '(1 2 3 4 5))
#t
>
|#

;;; intersection-set - this is O(n) in time
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
	      ((< x1 x2) (intersection-set (cdr set1) set2))
	      ((> x1 x2) (intersection-set set1 (cdr set2)))))))
#|
> (intersection-set '(1 2 3 4 5) '(3 4 5 6 7 8))
'(3 4 5)
> 
|#

;;; Exercise 2.61 - adjoin-set
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
	((element-of-set? x set) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))
#|
> (adjoin-set 3 '(1 2 4 5 6))
'(1 2 3 4 5 6)
> (adjoin-set 3 '())
'(3)
> (adjoin-set 3 '(1 2 3 4 5))
'(1 2 3 4 5)
> 
|#

;;; Exercise 2.61 - union-set - this is an O(n) implementation
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
		 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
		 ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
#|
> (union-set '(1 2 3) '(4 5 6))
'(1 2 3 4 5 6)
> (union-set '(1 2 3 4 5) '(3 4 5 6 7 8))
'(1 2 3 4 5 6 7 8)
> 
|#
