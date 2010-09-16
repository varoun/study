;;;; Branch and Bound searching -- Problem 19-5

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
  "Computes the straight line distance between two nodes"
  (let ((coord1 (get node1 'coordinates))
	(coord2 (get node2 'coordinates)))
    (sqrt (+ (expt (- (first coord1) (first coord2)) 2)
	     (expt (- (second coord1) (second coord2)) 2)))))

(defun path-length (path)
  "Computes the total straight line distance in a path"
  (if (endp (rest path))
      0
      (+ (straight-line-distance (first path) (second path))
	 (path-length (rest path)))))

(defun shorterp (path1 path2)
  "Returns true if the path lenght of the first path is lesser than the second"
  (< (path-length path1) (path-length path2)))

(defun extend-path (path)
  "Extends a path by consing on neighbor nodes that dont result in loops"
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if 
	   #'(lambda (neighbor) (member neighbor path))
	   (get (first path) 'neighbors))))
	 

(defun branch-and-bound (start finish 
			 &optional (queue (list (list start))))
  "Finds the shortest path from start to finish by sorting partial paths by path-length"
  (cond ((endp queue) nil)
	((eq finish (first (first queue)))
	 (reverse (first queue)))
	(t
	 (print queue)
	 (branch-and-bound
	  start
	  finish
	  (sort (append (extend-path (first queue))
			(rest queue))
		#'(lambda (p1 p2) (shorterp p1 p2)))))))
#|
CL-USER> (branch-and-bound 's 'f)

((S)) 
((D S) (A S)) 
((A S) (E D S) (A D S)) 
((E D S) (B A S) (A D S) (D A S)) 
((B A S) (A D S) (D A S) (F E D S) (B E D S)) 
((A D S) (D A S) (C B A S) (F E D S) (B E D S) (E B A S)) 
((D A S) (C B A S) (F E D S) (B A D S) (B E D S) (E B A S)) 
((C B A S) (F E D S) (B A D S) (B E D S) (E D A S) (E B A S)) 
(S D E F)
CL-USER> 
|#

   