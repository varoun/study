;;;; PERT Charts - 


;;; PERT Chart represented as a net
(setf (get 's 'successors) '(a d)
      (get 'a 'successors) '(b d)
      (get 'b 'successors) '(c e)
      (get 'c 'successors) '()
      (get 'd 'successors) '(e)
      (get 'e 'successors) '(f)
      (get 'f 'successors) '())

(setf (get 's 'time-consumed) 3
      (get 'a 'time-consumed) 2
      (get 'b 'time-consumed) 4
      (get 'c 'time-consumed) 3
      (get 'd 'time-consumed) 3
      (get 'e 'time-consumed) 2
      (get 'f 'time-consumed) 1)
;;;;

;;;;Problem 19-7

;;; Extending a path in the PERT net
(defun extend-path (path)
  "Extend a path by consing on successors, filter out loops"
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if #'(lambda (successor)
			 (member successor path))
		     (get (first path) 'successors))))
#|
CL-USER> (extend-path '(d a s))
((E D A S))
CL-USER> 
|#

;;; To get all paths through the net, incomplete paths 
;;; need to be sorted to the front of the queue.
(defun first-path-incomplete-p (p1 p2)
  (not (endp (extend-path p1)))) ; p2 not used
#|
CL-USER> (first-path-incomplete-p '(f e d s) '())
NIL
CL-USER> (first-path-incomplete-p '(e d s) '())
T
CL-USER> 
|#

;;; All paths from through the net from a given start
(defun all-paths (start
		  &optional (queue (list (list start))))
  (cond ((endp (extend-path (first queue)))
	 (mapcar #'reverse queue))
	(t
	 (print queue)
	 (all-paths
	  start
	  (sort (append (extend-path (first queue))
			(rest queue))
		#'first-path-incomplete-p)))))
#|
CL-USER> (all-paths 's)

((S)) 
((D S) (A S)) 
((A S) (E D S)) 
((E D S) (D A S) (B A S)) 
((B A S) (D A S) (F E D S)) 
((D A S) (E B A S) (C B A S) (F E D S)) 
((E B A S) (E D A S) (C B A S) (F E D S)) 
((E D A S) (F E B A S) (C B A S) (F E D S)) 
((S A D E F) (S A B E F) (S A B C) (S D E F))
CL-USER> 
|#

;;;; Problem 19-8

(defun time-consumed (path)
  "Compute the total time consumed in a path"
  (if (endp (rest path))
      0
      (+ (get (first path) 'time-consumed)
	 (get (second path) 'time-consumed)
	 (time-consumed (rest path)))))
#|
CL-USER> (time-consumed '(s a b e f))
20
CL-USER> 
|#

(defun longerp (p1 p2)
  "Take two paths, and return true if the first takes longer"
  (> (time-consumed p1) (time-consumed p2)))


(defun critical-path (start 
		      &optional (queue (list (list start))))
  "Return the longest path from a given start"
  (cond ((endp (extend-path (first queue)))
	 (reverse (first (sort queue #'longerp))))
	(t
	 (critical-path
	  start
	  (sort (append (extend-path (first queue))
			(rest queue))
		#'first-path-incomplete-p)))))
#|
CL-USER> (critical-path 's)
(S A B E F)
CL-USER> 
|#
