;;;; AnyPath Searches - DepthFirst/BreadthFirst/BestFirst

;;; Our search graph
;;; States are represented as symbols, links (to neighbours), and 
;;; coordinates of states as properties.

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

;;; ADT for search nodes
(defun node-state (search-node) 
  "The state of a search node is the first element of the list"
  (first search-node))
(defun update-node (new-state search-node)
  "Cons the new state to the search node"
  (cons new-state search-node))

;;; The visited list
;;; For initial prototyping, a list is used. This should be replaced
;;; by a hash-table for production use.
(defun make-visited-list (starting-state) 
  "Create a new visited list given a starting state"
  (list starting-state))

(defun visitedp (state visited-list)
  (member state visited-list))

(defun add-to-visited-list (state visited-list)
  (cons state visited-list))

;;; The search queue
(defun make-search-queue (starting-state)
  "A search queue consists of a list of search nodes"
  (list (list starting-state)))

(defun get-search-node (search-queue)
  "We always pick the first node from the queue in anypath searches"
  (first search-queue))


;;; Expanding a state
;;; This is most likey not the elegant way to do this. We ideally
;;; should not have to use setfs.
(defun expand (search-node visited-list)
  (let* ((neighbours (get (node-state search-node) 'neighbour))
	 (expanded-nodes '())
	 (new-visited-list visited-list))
    (dolist (state neighbours (values expanded-nodes new-visited-list))
      (unless (visitedp state new-visited-list)
	(setf expanded-nodes (cons (update-node state search-node) expanded-nodes))
	(setf new-visited-list (cons state new-visited-list))))))

;;; DepthFirstSearch -- DFS
(defun depth-first-search (start finish
			   &optional 
			   (queue (make-search-queue start))
			   (visited-list (make-visited-list start)))
  (format t "~%Queue:~a ||  Visited:~a" queue visited-list)
  (cond ((endp queue) nil)
	((eq (node-state (get-search-node queue)) finish)
	 (reverse (get-search-node queue)))
	(t
	 (multiple-value-bind (new-queue new-visited-list)
	     (expand (get-search-node queue) visited-list)
	   (depth-first-search start 
			       finish 
			       (append new-queue (rest queue))
			       new-visited-list)))))
#|
CL-USER> (depth-first-search 's 'f)

Queue:((S)) ||  Visited:(S)
Queue:((D S) (A S)) ||  Visited:(D A S)
Queue:((E D S) (A S)) ||  Visited:(E D A S)
Queue:((F E D S) (B E D S) (A S)) ||  Visited:(F B E D A S)
(S D E F)
CL-USER> 
|#

;;; BreadthFirst Search - BFS
(defun breadth-first-search (start 
			     finish
			     &optional
			     (queue (make-search-queue start))
			     (visited-list (make-visited-list start)))
  (format t "~%Queue:~a|Visited:~a" queue visited-list)
  (cond ((endp queue) nil)
	((eq (node-state (get-search-node queue)) finish)
	 (reverse (get-search-node queue)))
	(t
	 (multiple-value-bind (new-queue new-visited-list)
	     (expand (get-search-node queue) visited-list)
	   (breadth-first-search
	    start
	    finish
	    (append (rest queue) new-queue)
	    new-visited-list)))))
#|
CL-USER> (breadth-first-search 's 'f)

Queue:((S))|Visited:(S)
Queue:((D S) (A S))|Visited:(D A S)
Queue:((A S) (E D S))|Visited:(E D A S)
Queue:((E D S) (B A S))|Visited:(B E D A S)
Queue:((B A S) (F E D S))|Visited:(F B E D A S)
Queue:((F E D S) (C B A S))|Visited:(C F B E D A S)
(S D E F)
CL-USER> 
|#

;;; Best First Search - 

;; We need a heuristic function (as the crow flies distance) and a
;; sort function.
(defun straight-line-distance (state-1 state-2)
  "Given two states, calculate the distance between them using their
rectangular coordinates"
  (let ((x1 (first (get state-1 'coordinates)))
	(y1 (second (get state-1 'coordinates)))
	(x2 (first (get state-2 'coordinates)))
	(y2 (second (get state-2 'coordinates))))
    (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(defun closerp (state-1 state-2 goal-state)
  "Given two states and a goal state, return T if state-1 is closer to
the goal than state-2"
  (< (straight-line-distance state-1 goal-state)
     (straight-line-distance state-2 goal-state)))

(defun sort-search-nodes (search-queue sorting-fn goal-state)
  (sort search-queue
	(lambda (node-1 node-2) 
	  (funcall sorting-fn (node-state node-1) (node-state
  node-2) goal-state))))


;;; We should really not sort the whole queue, must use merge
;;; instead.
(defun best-first-search 
    (start 
     finish
     &optional
     (queue (make-search-queue start))
     (visited-list (make-visited-list start)))
  (format t "~%Queue:~a | Visited:~a" queue visited-list)
  (cond ((endp queue) nil)
	((eq (node-state (get-search-node queue)) finish)
	 (reverse (get-search-node queue)))
	(t
	 (multiple-value-bind (new-nodes updated-visited-list)
	     (expand (get-search-node queue) visited-list)
	   (best-first-search
	    start
	    finish
	    (sort-search-nodes (append new-nodes (rest queue))
			       #'closerp 
			       finish)
	    updated-visited-list)))))
