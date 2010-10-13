;;;; Informed Optimal Search - A*
;;;; This is our initial implementation.

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

;;; ADTs for search nodes, and search queues

;; The first element of a search node is the path length, the
;; rest is the partial path queue in reverse

(defun cost (search-node)
  (first search-node))

(defun partial-path (search-node)
  (rest search-node))

(defun state (search-node)
  (first (partial-path search-node)))

(defun make-search-queue (starting-state)
  (list (list 0 starting-state)))


;;; Straight line distance is used as the admissible heuristic,
;;; it is a valid underesitmate 
(defun straight-line-distance (state1 state2)
  (let ((x1 (first (get state1 'coordinates)))
	(y1 (second (get state1 'coordinates)))
	(x2 (first (get state2 'coordinates)))
	(y2 (second (get state2 'coordinates))))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))


;;; The sort function used to sort a queue by the cost
(defun sort-function (sorting-predicate goal)
  "Return a closure that can be used with the sort function"
  #'(lambda (node1 node2)
      (funcall sorting-predicate 
	       (+ (cost node1) 
		  (straight-line-distance (state node1) goal))
	       (+ (cost node2)
		  (straight-line-distance (state node2) goal)))))

;;; Merging two sorted search queues 
(defun merge-sorted-queues (queue1 queue2 goal)
  "Merge two sorted search queues"
  (merge 'list queue1 queue2 (sort-function #'< goal))) 

;;; Path length between two neighbouring states
(defun path-length (state1 state2)
  (straight-line-distance state1 state2))


;;; Expanding a search node to its neighbours
(defun expand (search-node)
  "Expand a search node to neighbours while preventing loops 
   locally, update the path length"
  (let ((pathcost (cost search-node))
	(partialpath (partial-path search-node)))
    (mapcar #'(lambda (neighbour)
		(cons (+ pathcost
			 (path-length neighbour (state search-node)))
		      (cons neighbour partialpath)))
	    (remove-if #'(lambda (state) (member state partialpath))
		       (get (state search-node) 'neighbour)))))


;;; A*
(defun a-star (start finish)
  (do ((queue (make-search-queue start) 
	      (merge-sorted-queues 
	       (sort (expand (first queue)) 
		     (sort-function #'< finish))
	       (rest queue) finish)))
      ((endp queue) nil)
    (format t "~%QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost")
    (dolist (node queue)
      (format t "~%(~a ~a ~a)" 
	      (partial-path node) 
	      (cost node) 
	      (+ (cost node) (straight-line-distance (state node) finish)))) 
    (when (eq (state (first queue)) finish)
      (return (reverse (first queue))))))

#|
CL-USER> (a-star 's 'f)

QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((S) 0 11.0)
QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((A S) 5.0 12.615773)
((D S) 4.2426405 12.786644)
QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((D S) 4.2426405 12.786644)
((B A S) 8.0 13.0)
((D A S) 11.082763 19.626766)
QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((B A S) 8.0 13.0)
((E D S) 7.2426405 13.073592)
((A D S) 10.325403 17.941177)
((D A S) 11.082763 19.626766)
QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((E D S) 7.2426405 13.073592)
((C B A S) 12.0 15.0)
((A D S) 10.325403 17.941177)
((D A S) 11.082763 19.626766)
((E B A S) 14.082763 19.913715)
QUEUE STAT:Partial-Path - Path-Length - Estimated Total Cost
((F E D S) 13.073592 13.073592)
((C B A S) 12.0 15.0)
((A D S) 10.325403 17.941177)
((B E D S) 13.325403 18.325403)
((D A S) 11.082763 19.626766)
((E B A S) 14.082763 19.913715)
(S D E F 13.073592)
CL-USER> 
|#
