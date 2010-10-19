;;;; A* with the use of an expanded list. 
;;;; We detect and correct inconsistent heuristics, the 
;;;; use of the expanded list is not strict.


;;; The search graph

(setf (get 's 'neighbour) '(a d)
      (get 'a 'neighbour) '(s b d)
      (get 'b 'neighbour) '(a c e)
      (get 'c 'neighbour) '(b)
      (get 'd 'neighbour) '(s a e)
      (get 'e 'neighbour) '(b d f)
      (get 'f 'neighbour) '(e))

(setf (get 's 'coordinates) '(0 3)
      (get 'a 'coordinates) '(4 6)
      (get 'b 'coordinates) '(7 6)
      (get 'c 'coordinates) '(11 6)
      (get 'd 'coordinates) '(3 0)
      (get 'e 'coordinates) '(6 0)
      (get 'f 'coordinates) '(11 3))

;;; ADTs for the search nodes and search queues
(defun path-cost (search-node)
  (first search-node))

(defun partial-path (search-node)
  (rest search-node))

(defun node-state (search-node)
  (first (partial-path search-node)))

(defun make-search-node (path-cost partial-path)
  (cons path-cost partial-path))


;;; Straight-Line distance between two states.
(defun straight-line-distance (state1 state2)
  (let ((coord1 (get state1 'coordinates))
	(coord2 (get state2 'coordinates)))
    (sqrt (+ (expt (- (first coord2) (first coord1)) 2)
	     (expt (- (second coord2) (second coord1)) 2)))))
#|
CL-USER> (straight-line-distance 's 'f)
11.0
CL-USER> 
|#

;;; Predicate to test if a state is present in the expanded-list
(defun state-in-expanded-p (state expanded-list)
  (member-if #'(lambda (node) (eq (node-state node) state))
	     expanded-list))

;;; Retrieving a node with a given state from the expanded list
(defun get-node-from-expanded (state expanded)
  (find-if #'(lambda (node) (eq (node-state node) state))
	   expanded))

#|
CL-USER> (state-in-expanded-p 'a '((2 d e s) (4 r e d) (5 f e d)))
NIL
CL-USER> (state-in-expanded-p 'a '((2 d e s) (5 a e s) (4 r e d) (5 f e d)))
((5 A E S) (4 R E D) (5 F E D))
CL-USER> (get-node-from-expanded 's '((2 a e s) (3 f e d) (5 d e c)))
NIL
CL-USER> (get-node-from-expanded 'f '((2 a e s) (3 f e d) (5 d e c)))
(3 F E D)
CL-USER> 
|#


;;; Expanding a node to its neighbours
;;; If we find a neighbour in the expanded list, we compare
;;; path costs to either discard the new node or delete the 
;;; old node from the expanded list.
(defun expand (node expanded-list)
  (let* ((new-nodes '())
	 (expanded expanded-list)
	 (neighbours (get (node-state node) 'neighbour)))
    (dolist (neighbour neighbours (values new-nodes expanded))
      (let ((new-node
	     (make-search-node 
	      (+ (straight-line-distance (node-state node) neighbour)
		 (path-cost node))
	      (cons neighbour (partial-path node)))))
	(if (state-in-expanded-p (node-state new-node) expanded)
	    (when (< (path-cost new-node)
		     (path-cost (get-node-from-expanded 
				 (node-state new-node) expanded)))
	      (push new-node new-nodes)
	      (setf expanded (remove (get-node-from-expanded
				      (node-state new-node) expanded)
				     expanded)))
	    (push new-node new-nodes))))))
#|
CL-USER> (expand '(5 s e) '((14 d e s)))
((9.2426405 D S E) (10.0 A S E))
NIL
CL-USER> (expand '(5 s e) '((4 d e s)))
((10.0 A S E))
((4 D E S))
CL-USER> 
|#

;;; Procedure to sort a queue by the estimated total cost
;;; Estimated total cost = path-cost + guess of remaining distance

(defun queue-sort-function (goal)
  #'(lambda (node1 node2)
      (< (+ (path-cost node1)
	    (straight-line-distance (node-state node1) goal))
	 (+ (path-cost node2)
	    (straight-line-distance (node-state node2) goal)))))

(defun sort-search-queue (search-queue goal)
  (sort search-queue (queue-sort-function goal)))
