;;;; Uniform Cost Search with Optimizations
;;;; We use the 'Dynamic Programming Optimality Principle'
;;;; In this specific case, we use a 'strict' expanded list.


;;;; The search graph
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

;;;; ADTs for the search nodes and search queues

(defun path-cost (search-node)
  (first search-node))

(defun partial-path (search-node)
  (rest search-node))

(defun node-state (search-node)
  (first (partial-path search-node)))

(defun search-node (search-queue)
  (first search-queue)) ; the queue is always 'sorted', we pick the first node in the queue!

(defun make-search-queue (starting-state)
  (list (list 0 starting-state)))

;;; We use the standard distance formula to compute the straight
;;; line distance between two states
(defun straight-line-distance (state1 state2)
  (let ((coord1 (get state1 'coordinates))
	(coord2 (get state2 'coordinates)))
    (sqrt (+ (expt (- (first coord2) (first coord1)) 2)
	     (expt (- (second coord2) (second coord1)) 2)))))

;;; Expanding a search node to its neighbours
;;; Note the use of an expanded list
(defun expand (node expanded-list)
  (if (member (node-state node) expanded-list)
      '() ; we dont expand a previously expanded state
      (let* ((state (node-state node))
	     (cost (path-cost node))
	     (partialpath (partial-path node))
	     (updated-expanded (cons state expanded-list))
	     (new-partial-paths '())
	     (neighbours 
	      (remove-if #'(lambda (state) 
			     (member state expanded-list))
			 (get state 'neighbour)))) ; we dont add previously expanded states to the node
	(dolist (neighbour neighbours 
		 (values new-partial-paths updated-expanded))
	  (setf new-partial-paths 
		(cons 
		 (cons (+ cost 
			  (straight-line-distance state neighbour))
		       (cons neighbour partialpath))
		 new-partial-paths))))))

;;; Procedures for search queue manipulation

(defun sorting-function (sort-by-predicate)
  #'(lambda (node1 node2)
      (funcall sort-by-predicate (path-cost node1) (path-cost node2))))
#|
CL-USER> (sort (expand '(7.2426405 E D S) '(d s)) (sorting-function #'<))
((13.073592 F E D S) (13.325403 B E D S))
CL-USER> (sort (expand '(7.2426405 E D S) '(d s)) (sorting-function #'>))
((13.325403 B E D S) (13.073592 F E D S))
CL-USER> 
|#

#|
;;; Keeping only the shortest path to a state 
;;; Only the shortest path should exist in either of the 
;;; two queues -  new-nodes/old-queue
;;; This looks quite complex - needs thought!
(defun remove-dups-queues (new-nodes old-queue)
  (labels ((iter (queue1 queue2 track-queue)
	     (cond ((endp track-queue) 
		    (values queue1 queue2))
		   ((member-if #'(lambda (n) 
				   (eq (node-state (first track-queue)) 
				       (node-state n)))
			       queue2)
		    (let* ((node1 (first track-queue))
			   (node2 (find-if #'(lambda (n) 
					       (eq (node-state n)
						   (node-state node1)))
					   queue2)))
		      (if (> (path-cost node1)
			     (path-cost node2))
			  (iter (remove node1 queue1) queue2 (rest track-queue))
			  (iter node1 (remove node2 queue2) (rest track-queue)))))
		   (t
		    (iter queue1 queue2 (rest track-queue))))))
    (iter new-nodes old-queue new-nodes)))


;;; Merging two sorted queues, making sure we keep only one 
;;; path (the shortest) to each state
(defun merge-queue (queue1 queue2)
  (multiple-value-bind (new-queue1 new-queue2)
      (remove-dups-queues queue1 queue2)
    (merge 'list
	   (sort new-queue1 (sorting-function #'<))
	   new-queue2
	   (sorting-function #'<))))


;;; UC with expanded list
(defun uc (start finish &optional
	   (queue (make-search-queue start))
	   (expanded '()))
  (format t "~%Queue:~a|Expanded:~a" queue expanded)
  (cond ((endp queue) nil)
	((eq (node-state (first queue)) finish)
	 (reverse (first queue)))
	(t
	 (multiple-value-bind (new-nodes new-expanded)
	     (expand (first queue) expanded)
	   (uc start 
	       finish
	       (merge-queue new-nodes (rest queue))
	       new-expanded)))))
#|
CL-USER> (uc 's 'f)

Queue:((0 S))|Expanded:NIL
Queue:((4.2426405 D S) (5.0 A S))|Expanded:(S)
Queue:((5.0 A S) (7.2426405 E D S))|Expanded:(D S)
Queue:((7.2426405 E D S) (8.0 B A S))|Expanded:(A D S)
Queue:((8.0 B A S) (13.073592 F E D S))|Expanded:(E A D S)
Queue:((12.0 C B A S) (13.073592 F E D S))|Expanded:(B E A D S)
Queue:((13.073592 F E D S))|Expanded:(C B E A D S)
(S D E F 13.073592)
CL-USER> 
|#
