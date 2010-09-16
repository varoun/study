;;;; Beam Search - Problem 19-6

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

;;; Straight-line distance between two nodes
(defun straight-line-distance (node1 node2)
  "Compute the distance between two nodes"
  (let ((coord1 (get node1 'coordinates))
	(coord2 (get node2 'coordinates)))
    (sqrt (+ (expt (- (first coord1) (first coord2)) 2)
	     (expt (- (second coord1) (second coord2)) 2)))))

;;; Is node1 closer to the target than node2
(defun closerp (node1 node2 target)
  "Is node1 closer to the target that node2"
  (< (straight-line-distance node1 target)
     (straight-line-distance node2 target)))

;;; Extending a path
(defun extend-path (path)
  (mapcar #'(lambda (new-node) (cons new-node path))
	  (remove-if 
	   #'(lambda (neighbor) (member neighbor path))
	   (get (first path) 'neighbors))))

;;; Beam search
(defun beam (start finish width 
	     &optional (queue (list (list start))))
  (setf queue (butlast queue (max (- (length queue) 
				     width) 
				  0))) ; trim queue
  (cond ((endp queue) nil)
	((eq (first (first queue)) finish)
	 (reverse (first queue)))
	(t
	 (print queue)
	 (beam start
	       finish
	       width
	       (sort 
		(apply #'append 
		       (mapcar #'extend-path queue)) ; extend all paths
		#'(lambda (p1 p2) (closerp (first p1)
					   (first p2)
					   finish)))))))
#|
CL-USER> (beam 's 'f 1)

((S)) 
((A S)) 
((B A S)) 
((C B A S)) 
NIL
CL-USER> (beam 's 'f 2)

((S)) 
((A S) (D S)) 
((B A S) (E D S)) 
(S D E F)
CL-USER> (beam 's 'f 3)

((S)) 
((A S) (D S)) 
((B A S) (E D S) (A D S)) 
(S D E F)
CL-USER> 
|#


		    
