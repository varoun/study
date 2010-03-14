;;;; Representing sets as a list 

;;; element-of-set?
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))
#|
> (element-of-set? 1 '(2 3 4 1 5))
#t
> (element-of-set? 'x '(a b c d))
#f
> 
|#

;;; adjoin-set
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
#|
> (adjoin-set 'a '(a b c d))
'(a b c d)
> (adjoin-set 'x '(a b c d))
'(x a b c d)
> 
|#

;;; intersection-set
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))
#|
> (intersection-set '(a b c) '(d e f))
'()
> (intersection-set '(a b c d e) '( x y z a b c))
'(a b c)
> 
|#

;;; Exercise 2.59 - union-set
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else (cons (car set1) (union-set (cdr set1) set2)))))
#|
> (union-set '(a b c d e) '(x y z a b c))
'(d e x y z a b c)
> 
|#

  