;;;; Measuring out water using two water crocks

;;; Problem 19-10

(defun transfer (x y a b d)
  (cond ((= x d) ; right amount in A
	 (format t "~%I can produce ~a units in A." d)
	 nil)
	((= y d) ; right amount in B
	 (format t "~%I can produce ~a units in B." d)
	 nil)
	((= x a) ; A is full -> empty it
	 (cons '(empty A) 
	       (transfer 0 y a b d)))
	((= y 0) ; B is empty -> fill it
	 (cons '(fill B)
	       (transfer x b a b d)))
	((> (- a x) y) ; will the amount in B fit in A
	 (cons '(empty B into A)
	       (transfer (+ x y) 0 a b d)))
	(t ; otherwise fill A from B
	 (cons '(fill A from B)
	       (transfer a (- y (- a x)) a b d)))))

;; This needs careful thinking. Water always goes from 
;; source -> B -> A. Anytime A is full, it gets emptied.
;; Anytime B is empty, it gets  filled from source etc....
;; Read Pg 287-288
#|
CL-USER> (transfer 0 0 3 5 2)

I can produce 2 units in B.
((FILL B) (FILL A FROM B))
CL-USER> (transfer 0 0 5 3 2)

I can produce 2 units in B.
((FILL B) (EMPTY B INTO A) (FILL B) (FILL A FROM B) (EMPTY A) (EMPTY B INTO A) (FILL B) (EMPTY B INTO A) (FILL B) (FILL A FROM B))
CL-USER> 
|#


	 
	 