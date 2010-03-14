;;;; Exercise 2.29 - Binary mobiles

;; A constructor for the binary mobile, which consists of a left and
;; right branch
(define (make-mobile left right)
  (list left right))

;; A constructor for a branch, which consists of a rod of certain length
;; from which hangs a weight or another mobile
(define (make-branch length structure)
  (list length structure))

;; Selectors for these constructors

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;; computing the weight of the binary mobile
(define (mobile-weight mobile)
  (let ((ls (branch-structure (left-branch mobile)))
	(rs (branch-structure (right-branch mobile))))
    (cond ((and (not (pair? ls)) (not (pair? rs)))
	   (+ ls rs))
	  ((and (pair? ls) (pair? rs))
	   (+ (mobile-weight ls) (mobile-weight rs)))
	  ((pair? rs)
	   (+ (mobile-weight rs) ls))
	  (else (+ rs (mobile-weight ls))))))

#|
> (define b1 (make-branch 1 1))
; no values returned
> (define b2 (make-branch 2 2))
; no values returned
> (define m1 (make-mobile b1 b2))
; no values returned
> (mobile-weight m1)
3
> (define b3 (make-branch 1 3))
; no values returned
> (define b4 (make-branch 1 m1))
; no values returned
> (define m2 (make-branch b4 b3))
; no values returned
> (mobile-weight m2)
6
>
|#

;; predicate to test if the mobile is balanced
(define (balanced? mobile)
  (let ((ls (branch-structure (left-branch mobile)))
	(rs (branch-structure (right-branch mobile)))
	(ll (branch-length (left-branch mobile)))
	(rl (branch-length (right-branch mobile))))
    (cond ((and (not (pair? ls)) (not (pair? rs)))
	   (= (* rs rl) (* ls ll)))
	  ((and (not (pair? ls)) (pair? rs))
	   (and (= (* ls ll) (* (mobile-weight rs) rl))
		(balanced? rs)))
	  ((and (pair? ls) (not (pair? rs)))
	   (and (= (* (mobile-weight ls) ll) (* rs rl))
		(balanced? ls)))
	  (else
	   (and (= (* (mobile-weight ls) ll) (* (mobile-weight rs) rl))
		(balanced? ls)
		(balanced? rs))))))
;; balanced? is not very easy to read, it would be better to define procedures for things
;; like branch-weight which returns the weight at the end of a branch by calling 
;; mobile-weight if needed, and branch-torque, which computes the torque as a product
;; of branch-length and branch-weight

#|
> (balanced? m2)
#f
> 
|#

;; Another change would be to define a predicate to check if the structure at the
;; end of a branch is a weight or another mobile. Currently, the programs uses pair?
;; directly to check for this, but defining an exclusive predicate for this would enable
;; flexibility later on in case the representation of mobiles needs changing.
