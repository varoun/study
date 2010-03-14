;;;; Huffman encoding trees
;;;; Section 2.3.4

;;; This program starts with the representation of the tree first!

;; leaves - constructors/selectors and predicates
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf object) (cadr object))
(define (weight-leaf object) (caddr object))

;; code tree (i.e the non-leaf part of the tree)
(define (make-code-tree left right)
  (list left 
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
	((= bit 1) (right-branch tree))
	(else (error "Error - bit is neither 1 nor 0"))))

;; The adjoin-set procedure
;; The set we are concerned about here is the set of leaves and trees 
;; which need to be successively merged by the least weights when the tree 
;; is generated. The adjoin-set takes an element (which can be a tree or leaf)
;; and inserts it into the set represented as a list ordered by the weights
(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

;; The make-leaf-set procedure
;; This takes a list of symbol-weight pairs like '((A 8) (B 3) (C 1)) and creates 
;; the initial ordered set ready to me merged according to the huffman tree gen algo
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((element (car pairs)))
	(adjoin-set (make-leaf (car element) (cadr element))
		    (make-leaf-set (cdr pairs))))))
#|
> (make-leaf-set '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1)))
'((leaf f 1) (leaf e 1) (leaf d 1) (leaf c 1) (leaf b 3) (leaf a 8))
> 
|#

;; Exercise 2.67
(define sample-tree 
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree (make-leaf 'B 2)
				  (make-code-tree (make-leaf 'C 1)
						  (make-leaf 'D 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

#|
> (decode sample-message sample-tree)
'(a c a b b d a)
> 
|#

;; Exercise 2.68 - Encoding a message 
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (cond ((not (memq s (symbols tree)))
	 (error "Symbol not in Huffman Encoding tree -- ENCODE-SYMBOL"))
	((leaf? tree) '())
	((memq s (symbols (left-branch tree)))
	 (cons 0 (encode-symbol s (left-branch tree))))
	((memq s (symbols (right-branch tree)))
	 (cons 1 (encode-symbol s (right-branch tree))))))
#|
> (encode '(a c a b b d a) sample-tree)
'(0 1 1 0 0 1 0 1 0 1 1 1 0)
> 
|#


;; Exercise 2.69 - Generating Huffman encoding trees from symbol-freq pairs

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (if (null? (cddr set)) ; if there are just two elements
      (make-code-tree (cadr set) (car set)) ; merge them by constructing a code tree
      (successive-merge 
       (adjoin-set (make-code-tree (cadr set) (car set)) (cddr set))))) ; make a code tree of
                                                                        ; 1st 2 elements, adjoin
                                                                        ; with rest, and successive
                                                                        ; merge the result

;; tracing successive-merge indicates a less than optimal end condition in the if statement ???

#|
> (encode '(b a c d a c) (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
'(0 1 1 0 0 0 0 0 1 1 0 0 0)
> (decode '(0 1 1 0 0 0 0 0 1 1 0 0 0)  (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
'(b a c d a c)
> 
|#
