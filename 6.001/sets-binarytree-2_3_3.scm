;;;; Representing sets as binary trees - these have O(log n) growth

;;; Constructors and selectors for trees
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

;;; Set operations

;; element-of-set?
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

;; adjoin-set
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set) 
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

#| adjoin-set can create a highly unbalanced binary tree
> (define a (adjoin-set 1 '()))
; no values returned
> a
'(1 () ())
> (define b (adjoin-set 2 a))
; no values returned
> b
'(1 () (2 () ()))
> (define c (adjoin-set 3 b))
; no values returned
> (define d (adjoin-set 4 c))
; no values returned
> (define e (adjoin-set 5 d))
; no values returned
> (define f (adjoin-set 6 e))
; no values returned
> (define g (adjoin-set 7 f))
; no values returned
> g
'(1 () (2 () (3 () (4 () (5 () (6 () (7 () ())))))))
> 
|#

;;; Exercise 2.63 - Two ways of converting a binary tree to a list 

(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left-branch tree))
	      (cons (entry tree) (tree->list1 (right-branch tree))))))
#|
> (tree->list1 g)
'(1 2 3 4 5 6 7)
> 
|#

(define (tree->list2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
	result
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

#|
> (tree->list2 g)
'(1 2 3 4 5 6 7)
> 
|#

;;; Exercise 2.64 - list->tree converts an ordered list to balanced binary tree
;;; This version uses let* in contrast to let used in the book to make the code easier to read

;; The helper procedure partial-tree
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
	     (left-result (partial-tree elts left-size))
	     (left-tree (car left-result))
	     (non-left-elts (cdr left-result))
	     (right-size (- n (+ left-size 1)))
	     (this-entry (car non-left-elts))
	     (right-result (partial-tree (cdr non-left-elts) right-size))
	     (right-tree (car right-result))
	     (remaining-elts (cdr right-result)))
	(cons (make-tree this-entry left-tree right-tree)
	      remaining-elts))))

(define (list->tree ordered-list)
  (car (partial-tree ordered-list (length ordered-list))))

#|
> (list->tree (tree->list1 g))
'(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))
>

To figure out how partial-tree works, examine the following interaction
> (list->tree '(1))
'(1 () ())
> (list->tree '(1 2))
'(1 () (2 () ()))
> (list->tree '(1 2 3))
'(2 (1 () ()) (3 () ()))
> (list->tree '(1 2 3 4))
'(2 (1 () ()) (3 () (4 () ())))
> (list->tree '(1 2 3 4 5))
'(3 (1 () (2 () ())) (4 () (5 () ())))
> 

Using substitution to work out the first few evals for lists '(1), '(1 2), '(1 2 3) 
etc will reveal the underlying mechanism. Partial-tree constructs a binary tree by
splitting the list into two, and forming a node by using the midpoint element as
the node entry, and construction the left and right branches by calling partial
tree on the elements to the left and right of the node entry.This recursive 
strategy will result in the terminal case of evaluating partial-tree of the 
form (partial-tree '(3) 1) which will eval to '(3 () ()).
|#
