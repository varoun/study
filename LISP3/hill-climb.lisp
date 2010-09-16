;;;; Hill Climbing search -- Problem 19-4

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
  "Compute the straight line distance between two nodes"
  (let ((coord1 (get node1 'coordinates))
	(coord2 (get node2 'coordinates)))
    (sqrt (+ (expt (- (first coord1) (first coord2)) 2)
	     (expt (- (second coord1) (second coord2)) 2)))))

(defun extend-path (path)
  "Extend the path by consing on new neighbor nodes that dont result in loops"
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if 
	   #'(lambda (neighbor) (member neighbor path))
	   (get (first path) 'neighbors))))

(defun closerp (node1 node2 target)
  "Returns true if node1 is closer to target than node2, else false"
  (< (straight-line-distance node1 target) 
     (straight-line-distance node2 target)))

(defun hill-climb (start finish
		   &optional (queue (list (list start))))
  "Perform hill climbing search for path from start to finish"
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t
	 (print queue)
	 (hill-climb 
	  start
	  finish
	  (append (sort (extend-path (first queue))
			#'(lambda (p1 p2) 
			    (closerp (first p1) (first p2) finish)))
		  (rest queue))))))
