;;;; Best First search

;;; The search net
(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

(setf (get 's 'coordinates) '(0 3)
      (get 'a 'coordinates) '(4 6)
      (get 'b 'coordinates) '(7 6)
      (get 'c 'coordinates) '(11 6)
      (get 'd 'coordinates) '(3 0)
      (get 'e 'coordinates) '(6 0)
      (get 'f 'coordinates) '(11 3))

(defun straight-line-distance (node1 node2)
  "Computes the straight line distance between two nodes, whose position is specified by x and y coordinates"
  (let ((coordinate1 (get node1 'coordinates))
	(coordinate2 (get node2 'coordinates)))
    (sqrt (+ (expt (- (first coordinate1)
		      (first coordinate2))
		   2)
	     (expt (- (second coordinate1)
		      (second coordinate2))
		   2)))))

(defun closerp (path1 path2 target-node)
  "Return true if node1 (first path1) is close to target than node2, false otherwise"
  (< (straight-line-distance (first path1) target-node)
     (straight-line-distance (first path2) target-node)))

(defun extend-path (path)
  "Extend the path by consing on a new neighbor node that does not result in loops"
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if #'(lambda (neighbor) (member neighbor path))
		     (get (first path) 'neighbors))))

(defun best-first-search (start finish 
			   &optional (queue (list (list start))))
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t 
	 (print queue)
	 (best-first-search start 
			     finish
			     (sort 
			      (append (extend-path (first queue))
				      (rest queue))
			      #'(lambda (p1 p2) 
				  (closerp p1 p2 finish)))))))
			     
#|
CL-USER> (best-first-search 's 'f)

((S)) 
((A S) (D S)) 
((B A S) (D A S) (D S)) 
((C B A S) (E B A S) (D A S) (D S)) 
((E B A S) (D A S) (D S)) 
(S A B E F)
CL-USER> 
|#

;;; Problem 19-3
(defun best-first-with-merge (start finish 
			      &optional (queue (list (list start))))
  "A more efficient version of best-first that does not sort previously sorted paths"
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t 
	 (print queue)
	 (best-first-with-merge 
	  start
	  finish
	  (merge 'list 
		 (sort (extend-path (first queue))
		       #'(lambda (p1 p2) (closerp p1 p2 finish)))
		 (rest queue)
		 #'(lambda (p1 p2) (closerp p1 p2 finish)))))))
#|
CL-USER> (best-first-with-merge 's 'f)

((S)) 
((A S) (D S)) 
((B A S) (D A S) (D S)) 
((C B A S) (E B A S) (D A S) (D S)) 
((E B A S) (D A S) (D S)) 
(S A B E F)
CL-USER> 
|#

