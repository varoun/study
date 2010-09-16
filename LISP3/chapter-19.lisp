;;;; Chapter 19 - Search

;;; The search net
(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

;;; Extending a partial path
(defun extend (path)
  (print (reverse path))
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if #'(lambda (neighbor) (member neighbor path))
		     (get (first path) 'neighbors))))

;;; Depth First search
(defun depth-first (start finish 
		    &optional (queue (list (list start))))
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t
	 (depth-first start 
		      finish
		      (append (extend (first queue))
			      (rest queue))))))
#|
CL-USER> (extend '(d a s))

(S A D) 
((E D A S))
CL-USER> (depth-first 's 'f)

(S) 
(S A) 
(S A B) 
(S A B C) 
(S A B E) 
(S A B E D) 
(S A B E F)
CL-USER> 
|#

#| manually calling extend
CL-USER> (extend '(s))
(S) 
((A S) (D S))
CL-USER> (extend '(a s))
(S A) 
((B A S) (D A S))
CL-USER> (extend '(b a s))
(S A B) 
((C B A S) (E B A S))
CL-USER> (extend '(c b a s))
(S A B C) 
NIL
CL-USER> (extend '(e b a s))
(S A B E) 
((D E B A S) (F E B A S))
CL-USER> 
|#

;;; Breadth first search

(defun breadth-first (start finish 
		      &optional (queue (list (list start))))
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t 
	 (breadth-first start 
			finish
			(append (rest queue)
				(extend (first queue)))))))
#|
CL-USER> (breadth-first 's 'f)

(S) 
(S A) 
(S D) 
(S A B) 
(S A D) 
(S D A) 
(S D E) 
(S A B C) 
(S A B E) 
(S A D E) 
(S D A B) 
(S D E B) 
(S D E F)
CL-USER> 
|#

